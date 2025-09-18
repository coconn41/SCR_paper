#####
# Settings:
#####
use_cached_LC = TRUE
Conductance_resistance = "Resistance"
calcPC=FALSE
cluster = TRUE
if(cluster==TRUE){setwd('/user/collinoc/SCR_paper/')
  options(graph4lg.path_graphab = "/user/collinoc/graphab-3.0.5.jar")}
use_cached_Rdata = TRUE
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
# Create projects:
#####
print(paste0("Creating projects"))
for(i in 1:nrow(wmus)){
wmu_i = wmus[i,]
proj_name = wmu_i$UNIT
proj_name_string = paste0(proj_name,".tif")

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
}
#####
# Create graphab links:
#####
print(paste0("Creating links"))
for(i in 1:nrow(wmus)){
wmu_i = wmus[i,]
proj_name = wmu_i$UNIT
r <- terra::rast(paste0(getwd(),'/Scripts/PC_graph4lg/WMU_rasts/',proj_name,".tif"))

graphab_links_fixed(proj_name = paste0("proj_",proj_name),
                    proj_path = paste0(getwd(),"/Graphab_projects"),
                    distance = "cost",
                    cost = paste0(getwd(),'/Scripts/PC_graph4lg/WMU_rasts/',proj_name,'.tif'),
                    name = "linkset_1",
                    topo = "planar")
}
#####
# Create graphs:
#####
print(paste0("Creating graphs"))
for(i in 1:nrow(wmus)){
  wmu_i = wmus[i,]
  proj_name = wmu_i$UNIT
  graphab_graph_fixed(proj_name = paste0("proj_",proj_name),
                      proj_path = paste0(getwd(),"/Graphab_projects"),
                      linkset = "linkset_1",
                      name = "graph_1",
                      thr = 1675)
}
#####
# Calculate PC index:
#####
if(calcPC==TRUE){
print(paste0("Calculting PC index"))
for(i in 1:nrow(wmus)){
  wmu_i = wmus[i,]
  proj_name = wmu_i$UNIT
  start_time = Sys.time()
  met <- graphab_metric_fixed(proj_name = paste0("proj_",proj_name),
                 proj_path = paste0(getwd(),"/Graphab_projects"),
                 graph = "graph_1",
                 metric = "PC",
                 dist = 1675,
                 prob = 0.05,
                 beta = 1,
                 cost_conv = FALSE)
  end_time = Sys.time()
  diff = end_time-start_time
  df = data.frame(WMU = proj_name,
                  PC = met[[2]]$PC,
                  time_diff = diff)
  if(i==1){df2 = df}
  if(i>1){df2 = rbind(df,df2)}
  write.csv(df2,file = paste0(getwd(),'/Scripts/PC_graph4lg/Output/PC_results.csv'))
  }
}
#####
# Calculate delta PC index:
#####
print(paste0("Calculating dPC index"))
for(i in 1:nrow(wmus)){
  wmu_i = wmus[i,]
  proj_name = wmu_i$UNIT
  print(paste0("Calculating dPC for ",proj_name))
  if(file.exists(paste0(getwd(),'/Scripts/PC_graph4lg/Output/dPC/dPC_results_',
                        proj_name,'.csv'))==T){next}
  start_time = Sys.time()
  dPC_met <- graphab_metric_fixed(proj_name = paste0("proj_",proj_name),
                              proj_path = paste0(getwd(),"/Graphab_projects"),
                              graph = "graph_1",
                              metric = "dPC",
                              dist = 1675,
                              prob = 0.05,
                              beta = 1,
                              cost_conv = FALSE)
  end_time = Sys.time()
  diff = end_time-start_time
  df = data.frame(WMU = rep(x = proj_name,length(dPC_met[[2]]$Id)),
                  Id = dPC_met[[2]]$Id,
                  d_PCIntra = dPC_met[[2]]$d_PCIntra,
                  d_PCFlux = dPC_met[[2]]$d_PCFlux,
                    d_PCCon = dPC_met[[2]]$d_PCCon,
                  time_diff = rep(x=diff,length(dPC_met[[2]]$Id)))
  write.csv(df,
            file = paste0(getwd(),
                          '/Scripts/PC_graph4lg/Output/dPC/dPC_results_',
                          proj_name,'.csv'))
  }

# Write compiler
#####
# PC index map and distribution:
#####
# ggplot(data = df2,
#        aes(x=PC))+
#   geom_histogram()+
#   theme_classic()
# 
# wmus_index = left_join(wmus,df2 %>%
#                          rename(UNIT = 'WMU'))
# 
# tm_shape(wmus_index)+
#   tm_polygons(col='PC',
#               breaks = c(0,.25,.5,.75,1),
#               legend.hist=T,
#               palette = get_brewer_pal("Greens",n=4))+
#   tm_layout(legend.outside = T)
