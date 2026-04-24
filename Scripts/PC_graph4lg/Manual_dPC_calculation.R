#####
# Settings:
#####
use_cached_LC = TRUE
Conductance_resistance = "Resistance"
calcPC=FALSE
cluster = FALSE
if(cluster==TRUE){setwd('/user/collinoc/SCR_paper/')
  options(graph4lg.path_graphab = "/user/collinoc/graphab-3.0.5.jar")}
use_cached_Rdata = FALSE
#####
# Load libraries:
#####
source(paste0(getwd(),'/Scripts/Universal/Load_libraries.R'))
#####
# WMU Shapefile and Land Cover Download:
#####
wmus = read_sf(paste0(getwd(),"/Data/Input_data/WMUs/Wildlife_Management_Units.shp"))
template = read_sf(paste0(getwd(),'/Data/Input_data/cb_2018_us_state_500k.shp')) %>%
  filter(NAME == "New York") %>%
  st_transform(.,crs = 32618)
if(use_cached_Rdata==F){
  if(use_cached_LC==FALSE){source(paste0(getwd(),"/Scripts/Universal/Land_cover_download.R"))}
  if(use_cached_LC==TRUE){
    #if(cluster==TRUE){LC = raster::raster(paste0(getwd(),'/Data/Input_data/NLCD_NLCD_Land_Cover_2019.tif'))}
    #if(cluster==FALSE){LC = raster::raster(paste0(getwd(),'/Data/Input_data/NYS NLCD_NLCD_Land_Cover_2019.tif'))}
    LC = raster::raster(paste0(getwd(),'/Data/Input_data/NYS NLCD_NLCD_Land_Cover_2019.tif'))
    LCr = rast(LC)
    LCproj = terra::project(LCr,template)
    LCcrop = terra::crop(x = LCproj,
                         y = vect(template),
                         mask = T)}
  #####
  # Process alpine ecozone:
  #####
  source(paste0(getwd(),'/Scripts/SCR/Alpine_ecozone_cleaning.R'))
  #####
  # Process roadways:
  #####
  source(paste0(getwd(),'/Scripts/SCR/Roadway_data_cleaning.R'))
  #####
  # Process raster:
  #####
  source(paste0(getwd(),'/Scripts/SCR/Resistance_raster_processing.R'))
  writeRaster(Resistance_grid,
              filename=paste0(getwd(),'/Data/Input_data/Resistance_grid.tiff'),
              overwrite=T)
}
#####
# Fix graphab functions:
#####
source(paste0(getwd(),'/Scripts/PC_graph4lg/patch_graphab_project.R'))
source(paste0(getwd(),'/Scripts/PC_graph4lg/patch_graphab_links.R'))
source(paste0(getwd(),'/Scripts/PC_graph4lg/patch_graphab_graph.R'))
source(paste0(getwd(),'/Scripts/PC_graph4lg/patch_graphab_metric.R'))
#####
# Cache R session:
#####
if(use_cached_Rdata==TRUE){Resistance_grid <- terra::rast(paste0(getwd(),'/Data/Input_data/Resistance_grid.tiff'))}
#####
# Create all projects
#####
Node_locs <- read_csv("Scripts/PC_graph4lg/Input/Loc_metric_table_w_private.csv") %>%
  st_as_sf(.,coords=c('longitude','latitude')) %>%
  st_set_crs(.,4326) %>%
  st_transform(.,32618)
wmus <- wmus %>%
  st_transform(.,crs=32618)
for(i in 1:nrow(wmus)){
  wmu_i <- wmus[i,]
  wmu_nodes = st_intersection(wmu_i,Node_locs)
  print(paste0(i," of 92"))
  proj_name = wmu_i$UNIT
  proj_name_string = paste0(proj_name,"_main.tif")
  
  r = terra::crop(Resistance_grid,
                  vect(wmu_i %>%
                         st_transform(.,crs=32618)),
                  mask = T)
  
  r_int <- round(r)
  r_int <- as.int(r_int)
  r_int[is.na(r_int)]=10000
  
  writeRaster(r_int,
              filename = paste0(getwd(),'/Scripts/PC_graph4lg/WMU_rasts/',proj_name,".tif"),
              datatype = "INT2S",
              overwrite = TRUE)
  
  graphab_project_fixed(proj_name = paste0("proj_",wmu_i$UNIT),
                        proj_path = paste0(getwd(),"/Graphab_projects"),
                        raster = paste0(getwd(),'/Scripts/PC_graph4lg/WMU_rasts/',wmu_i$UNIT,'.tif'),
                        habitat = c(1),
                        minarea = .017)
  graphab_links_fixed(proj_name = paste0("proj_",proj_name),
                      proj_path = paste0(getwd(),"/Graphab_projects"),
                      distance = "cost",
                      cost = paste0(getwd(),'/Scripts/PC_graph4lg/WMU_rasts/',proj_name,'.tif'),
                      name = "linkset_1",
                      topo = "planar")
  graphab_graph_fixed(proj_name = paste0("proj_",proj_name),
                      proj_path = paste0(getwd(),"/Graphab_projects"),
                      linkset = "linkset_1",
                      name = "graph_1",
                      thr = 1675)
  met <- graphab_metric_fixed(proj_name = paste0("proj_",proj_name),
                              proj_path = paste0(getwd(),"/Graphab_projects"),
                              graph = "graph_1",
                              metric = "PC",
                              dist = 1675,
                              prob = 0.05,
                              beta = 1,
                              cost_conv = FALSE)
  desc=graphab_project_desc(proj_name = paste0("proj_",wmu_i$UNIT),
                            linkset = 'linkset_1',
                            proj_path = paste0(getwd(),'/Graphab_projects/'))
  df = data.frame(WMU = proj_name,
                  PC = met[[2]]$PC,
                  loc_name = "All",
                  Location_ID = -999,
                  num_patches = desc$`Number of patches`)
  if(i==1){df2 = df}
  if(i>1){df2 = rbind(df,df2)}
  for(j in 1:nrow(wmu_nodes)){
    if(nrow(wmu_nodes)==0){break}
    print(paste0(j," of ",nrow(wmu_nodes)," in WMU ",i," of 92"))
    wmu_node_j = wmu_nodes[j,]
    proj_name_string = paste0(proj_name,"_",j,".tif")
    r = terra::crop(Resistance_grid,
                    vect(wmu_i %>%
                           st_transform(.,crs=32618)),
                    mask = T)
    
    polys = landscapemetrics::get_patches(r,directions = 4)$layer_1$class_1 %>%
      as.polygons() %>%
      st_as_sf()
    
    poly_int_idx = st_intersects(wmu_node_j,polys) %>%
      unlist()
    if(length(poly_int_idx)==0){
      poly_int_idx = st_nearest_feature(wmu_node_j,polys)
    }
    poly_int = polys[poly_int_idx,]
    
    poly_int_rast = raster::rasterize(poly_int,r)
    poly_int_rast[poly_int_rast$layer==1]=1.5

    r_int_j = cover(poly_int_rast,r)
    
    r_int_j <- round(r_int_j)
    r_int_j <- as.int(r_int_j)
    r_int_j[is.na(r_int_j)]=10000
    
    writeRaster(r_int_j,
                filename = paste0(getwd(),'/Scripts/PC_graph4lg/WMU_rasts/',proj_name_string),
                datatype = "INT2S",
                overwrite = TRUE)
    
    graphab_project_fixed(proj_name = paste0("proj_",wmu_i$UNIT,"_",j),
                          proj_path = paste0(getwd(),"/Graphab_projects"),
                          raster = paste0(getwd(),'/Scripts/PC_graph4lg/WMU_rasts/',wmu_i$UNIT,'_',j,'.tif'),
                          habitat = c(1),
                          minarea = .017)
    graphab_links_fixed(proj_name = paste0("proj_",wmu_i$UNIT,"_",j),
                        proj_path = paste0(getwd(),"/Graphab_projects"),
                        distance = "cost",
                        cost = paste0(getwd(),'/Scripts/PC_graph4lg/WMU_rasts/',wmu_i$UNIT,'_',j,'.tif'),
                        name = "linkset_1",
                        topo = "planar")
    graphab_graph_fixed(proj_name = paste0("proj_",wmu_i$UNIT,"_",j),
                        proj_path = paste0(getwd(),"/Graphab_projects"),
                        linkset = "linkset_1",
                        name = "graph_1",
                        thr = 1675)
    met <- graphab_metric_fixed(proj_name = paste0("proj_",wmu_i$UNIT,"_",j),
                                proj_path = paste0(getwd(),"/Graphab_projects"),
                                graph = "graph_1",
                                metric = "PC",
                                dist = 1675,
                                prob = 0.05,
                                beta = 1,
                                cost_conv = FALSE)
    desc = graphab_project_desc(proj_name = paste0("proj_",wmu_i$UNIT,"_",j),
                                linkset = "linkset_1",
                                proj_path = paste0(getwd(),'/Graphab_projects/'))
    df = data.frame(WMU = paste0("proj_",wmu_i$UNIT,"_",j),
                    PC = met[[2]]$PC,
                    loc_name = wmu_node_j$loc_name,
                    Location_ID = wmu_node_j$Location_ID,
                    num_patches = desc$`Number of patches`)
    df2 = rbind(df,df2)
  }
}
write.csv(df2,paste0(getwd(),'/Scripts/PC_graph4lg/Output/dPC_raw.csv'))

df3 = df2 %>%
  filter(loc_name=="All") %>%
  left_join(.,desc_df2) %>%
  select(-c(loc_name,Location_ID)) %>%
  rename(num_patches_main = 'num_patches',
         main_WMU = 'WMU',
         WMU_PC = "PC")

df4 = df2 %>%
  filter(loc_name!="All") %>%
  mutate(main_WMU = substring(WMU,6,7),
         WMU = substring(WMU,6,12)) %>%
  rename(Site_PC = "PC") %>%
  left_join(.,desc_df2)

final_check_df = left_join(df4,df3,by='main_WMU') %>%
  mutate(dPC = (1-(Site_PC/WMU_PC))*100) %>%
  left_join(.,Node_locs %>%
              select(-c(`...1`,metric,area)))
final_check_df_sf = final_check_df %>%
  st_set_geometry(.,value='geometry') 
final_check_df = final_check_df %>%
  select(-geometry)


write.csv(final_check_df,paste0(getwd(),'/Scripts/PC_graph4lg/Output/dPC_final.csv'))
write_sf(final_check_df_sf,paste0(getwd(),'/Scripts/PC_graph4lg/Output/dPC_final_sf.shp'))

tm_shape(LCcrop)+
  tm_raster()
tm_shape(wmus)+
  tm_borders()+
  tm_shape(final_check_df_sf %>%
             arrange(dPC) %>%
             mutate(l_dPC = sqrt(dPC+0.00001)))+
  tm_dots(col='l_dPC',size=.5,shape=21,style='cont')

################################################ Everything below no longer needed.

#####
# Error correction:
#####
for(i in 1:nrow(wmus)){
wmu_i = wmus[i,]
wmu_nodes = st_intersection(wmu_i,Node_locs)
desc=graphab_project_desc(proj_name = paste0("proj_",wmu_i$UNIT),
                              linkset = 'linkset_1',
                              proj_path = paste0(getwd(),'/Graphab_projects/'))
num = desc$`Number of patches`
desc_df = data.frame(WMU = wmu_i$UNIT,
                     num_patches = num)
if(i == 1){desc_df2 = desc_df}
if(i > 1){desc_df2 = rbind(desc_df,desc_df2)}
for(j in 1:nrow(wmu_nodes)){
  if(nrow(wmu_nodes)==0){break}
  wmu_j = wmu_nodes[j,]
  desc = graphab_project_desc(proj_name = paste0("proj_",wmu_i$UNIT,"_",j),
                              linkset = "linkset_1",
                              proj_path = paste0(getwd(),'/Graphab_projects/'))
  num = desc$`Number of patches`
  desc_df = data.frame(WMU = paste0(wmu_i$UNIT,"_",j),
                       num_patches = num)
  desc_df2 = rbind(desc_df,desc_df2)
  }
}
#####
# Merge with df2 and check errors
#####
df2 = read.csv(paste0(getwd(),'/Scripts/PC_graph4lg/Output/dPC_raw.csv')) %>%
  select(-c(X,X.1))
names(df2)



sites_to_redo = final_check_df %>%
  filter(num_patches_main == num_patches)

dont_redo_df = df2 %>%
  filter(!c(loc_name %in% sites_to_redo$loc_name))



#####
# Redo Sites by switching to is within logic:
#####
ind = 0
for(i in 1:nrow(wmus)){
  wmu_i <- wmus[i,]
  wmu_nodes = st_intersection(wmu_i,Node_locs)
  proj_name = wmu_i$UNIT
  print(paste0(i," of 92"))
  for(j in 1:nrow(wmu_nodes)){
    if(nrow(wmu_nodes)==0){break}
    if(!(wmu_nodes[j,]$loc_name%in%sites_to_redo$loc_name)){next}
    ind = ind+1
    wmu_node_j = wmu_nodes[j,]
    proj_name_string = paste0(proj_name,"_",j,".tif")
    r = terra::crop(Resistance_grid,
                    vect(wmu_i %>%
                           st_transform(.,crs=32618)),
                    mask = T)
    
    polys = landscapemetrics::get_patches(r,directions = 4)$layer_1$class_1 %>%
      as.polygons() %>%
      st_as_sf()
    
    poly_int_idx = st_intersects(wmu_node_j,polys) %>%
      unlist()
    if(length(poly_int_idx)==0){
      poly_int_idx = st_nearest_feature(wmu_node_j,polys)
    }
    poly_int = polys[poly_int_idx,]
    
    poly_int_rast = raster::rasterize(poly_int,r)
    poly_int_rast[poly_int_rast$layer==1]=1.5
    
    r_int_j = cover(poly_int_rast,r)
    
    r_int_j <- round(r_int_j)
    r_int_j <- as.int(r_int_j)
    r_int_j[is.na(r_int_j)]=10000
    
    writeRaster(r_int_j,
                filename = paste0(getwd(),'/Scripts/PC_graph4lg/WMU_rasts/',proj_name_string),
                datatype = "INT2S",
                overwrite = TRUE)
    
    graphab_project_fixed(proj_name = paste0("proj_",wmu_i$UNIT,"_",j),
                          proj_path = paste0(getwd(),"/Graphab_projects"),
                          raster = paste0(getwd(),'/Scripts/PC_graph4lg/WMU_rasts/',wmu_i$UNIT,'_',j,'.tif'),
                          habitat = c(1),
                          minarea = .017)
    graphab_links_fixed(proj_name = paste0("proj_",wmu_i$UNIT,"_",j),
                        proj_path = paste0(getwd(),"/Graphab_projects"),
                        distance = "cost",
                        cost = paste0(getwd(),'/Scripts/PC_graph4lg/WMU_rasts/',wmu_i$UNIT,'_',j,'.tif'),
                        name = "linkset_1",
                        topo = "planar")
    graphab_graph_fixed(proj_name = paste0("proj_",wmu_i$UNIT,"_",j),
                        proj_path = paste0(getwd(),"/Graphab_projects"),
                        linkset = "linkset_1",
                        name = "graph_1",
                        thr = 1675)
    met <- graphab_metric_fixed(proj_name = paste0("proj_",wmu_i$UNIT,"_",j),
                                proj_path = paste0(getwd(),"/Graphab_projects"),
                                graph = "graph_1",
                                metric = "PC",
                                dist = 1675,
                                prob = 0.05,
                                beta = 1,
                                cost_conv = FALSE)
    redodf = data.frame(WMU = paste0("proj_",wmu_i$UNIT,"_",j),
                    PC = met[[2]]$PC,
                    loc_name = wmu_node_j$loc_name,
                    Location_ID = wmu_node_j$Location_ID)
    if(ind == 1){redodf2 = redodf}
    if(ind>1){redodf2 = rbind(redodf,redodf2)}
  }
}

write.csv(redodf2,paste0(getwd(),'/Scripts/PC_graph4lg/Output/dPC_redo_raw.csv'))

#####
# Check round 2
#####

for(i in 1:nrow(wmus)){
  wmu_i = wmus[i,]
  wmu_nodes = st_intersection(wmu_i,Node_locs)
  desc=graphab_project_desc(proj_name = paste0("proj_",wmu_i$UNIT),
                            linkset = 'linkset_1',
                            proj_path = paste0(getwd(),'/Graphab_projects/'))
  num = desc$`Number of patches`
  desc_df = data.frame(WMU = wmu_i$UNIT,
                       num_patches = num)
  if(i == 1){desc_df2 = desc_df}
  if(i > 1){desc_df2 = rbind(desc_df,desc_df2)}
  for(j in 1:nrow(wmu_nodes)){
    if(nrow(wmu_nodes)==0){break}
    wmu_j = wmu_nodes[j,]
    desc = graphab_project_desc(proj_name = paste0("proj_",wmu_i$UNIT,"_",j),
                                linkset = "linkset_1",
                                proj_path = paste0(getwd(),'/Graphab_projects/'))
    num = desc$`Number of patches`
    desc_df = data.frame(WMU = paste0(wmu_i$UNIT,"_",j),
                         num_patches = num)
    desc_df2 = rbind(desc_df,desc_df2)
  }
}

redodf3 = redodf2 %>%
  mutate(WMU = substring(WMU,6,11)) %>%
  distinct(WMU,.keep_all = T) %>%
  left_join(.,desc_df2,by='WMU') %>%
  mutate(main_WMU = substring(WMU,1,2)) %>%
  left_join(.,df3,by='main_WMU')

redodf4 = df2 %>%
  filter(loc_name=="All") %>%
  left_join(.,desc_df2) %>%
  select(-c(loc_name,Location_ID)) %>%
  rename(num_patches_main = 'num_patches',
         main_WMU = 'WMU',
         WMU_PC = "PC")

redodf5 = redodf3 %>%
  filter(loc_name!="All") %>%
  mutate(main_WMU = substring(WMU,6,7),
         WMU = substring(WMU,6,12)) %>%
  rename(Site_PC = "PC") %>%
  left_join(.,desc_df2)

final_check_df = left_join(df4,df3,by='main_WMU')

sites_to_redo = final_check_df %>%
  filter(num_patches_main == num_patches)

dont_redo_df = df2 %>%
  filter(!c(loc_name %in% sites_to_redo$loc_name))
