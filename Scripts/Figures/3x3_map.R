#####
# Load libraries:
#####
source(paste0(getwd(),'/Scripts/Universal/Load_libraries.R'))

#####
# Set calculation parameters:
#####
cores = 4
distance = 1675 # For home-range buffering and SCR threshold distance

#####
# Load county data:
#####
tdir=tempdir()

countyurl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_500k.zip"


if(file.exists(paste(tdir,"/cb_2018_us_county_500k.shp",sep=""))==F){
  download.file(countyurl,destfile = file.path(tdir,"Counties.zip"))
  unzip(file.path(tdir,"Counties.zip"),exdir=tdir)}

USA_Counties=read_sf(paste(tdir,"/cb_2018_us_county_500k.shp",sep=""))

Albany = USA_Counties %>%
  filter(NAME=="Albany" & STATEFP == 36) %>%
  st_transform(.,crs=32618)

#####
# Load forests:
#####
template = Albany
source(paste0(getwd(),'/Scripts/Universal/Land_cover_download.R'))

#####
# Process nodes:
#####
source(paste0(getwd(),'/Scripts/Universal/Process_nodes.R'))
nodes = wmu_nodes
remove(wmu_nodes)
fin_poly = fin_poly %>%
  rename(layer = 'lyr.1')
#####
# Roadway Data Cleaning:
#####
source(paste0(getwd(),"/Scripts/SCR/Roadway_data_cleaning.R"))

#####
# Alpine Ecozone Data Cleaning:
#####
source(paste0(getwd(),"/Scripts/SCR/Alpine_ecozone_cleaning.R"))

#####
# Process resistance Raster:
#####
source(paste0(getwd(),'/Scripts/SCR/Resistance_raster_processing.R'))

#####
# Calculate comparison matrix:
#####
source(paste0(getwd(),'/Scripts/SCR/Within_distance_comps.R'))

#####
# Least-cost Path Calculations:
#####
nodes = nodes %>%
  rename(layer = 'lyr.1')
source(paste0(getwd(),"/Scripts/SCR/Calculate_least_cost_paths.R"))

#####
# Get Euclidean distances:
#####
# This includes some reassigning of objects
# that are used in both landscape and patch level metrics
wmu_nodes = nodes
wmus = template
source(paste0(getwd(),"/Scripts/SCR/Get_euclidean_distances.R"))
remove(wmu_nodes)
remove(wmus)

#####
# Calculate threshold buffers:
#####
source(paste0(getwd(),"/Scripts/SCR/Calculate_buffer_thresholds.R"))

#####
# Get final dataset
#####

names(sinuosity_df)[1]="layer"
names(sinuosity_df)[2]="layer.1"

combdf = left_join(clipped_polys %>% st_drop_geometry(),
                   sinuosity_df %>% st_drop_geometry()) %>%
  #dplyr::select(-geometry) %>%
  left_join(.,buffer_polys %>%
              st_drop_geometry() %>%
              dplyr::select(layer,i_area,di_area)) %>%
  filter(is.na(inv_sinuosity)==F)

merged_df = combdf %>%
  mutate(sin_area = j_area*inv_sinuosity) %>%
  group_by(layer) %>%
  # summarize(i_a = max(i_area),
  #           s_sina = sum(sin_area),
  #           max_di = max(di_area),
  #           metric = (max(i_area)+sum(sin_area))/max(di_area))
  summarize(metric = (max(i_area)+sum(sin_area))/max(di_area)) %>%
  left_join(.,fin_poly)%>%#,by=c("origin_ID"="layer")) %>%
  st_set_geometry('geometry')

#####
# Make map
#####

plotdf = combdf %>%
  #rename(layer = 'lyr.1') %>%
  group_by(layer) %>%
  summarize(mean_inv_sinuousity = mean(inv_sinuosity,na.rm=T)) %>%
  left_join(.,fin_poly ) %>%
  #st_set_geometry('geometry') %>%
  #rename(origin_ID = 'layer') %>%
  left_join(.,merged_df ,by='layer') %>%
  mutate(area_cat = ifelse(area.x>=as.numeric(quantile(.$area.x,.9)),
                           3,ifelse(area.x>=as.numeric(quantile(.$area.x,.75)),2,1)),
         conn_cat = ifelse(mean_inv_sinuousity>=as.numeric(quantile(.$mean_inv_sinuousity,.9)),
                           3,ifelse(mean_inv_sinuousity>=as.numeric(quantile(.$mean_inv_sinuousity,.5)),2,1))) %>%
  rename(origin_ID='layer')

rm_polys = c(75,106,936,6030,1516,26,149,97,2194,91)

medians = plotdf %>%
  filter(!c(origin_ID%in%rm_polys)) %>%
  st_drop_geometry() %>%
  dplyr::select(-geometry.y) %>%
  left_join(.,plotdf %>%
              st_drop_geometry() %>%
              dplyr::select(-geometry.y)) %>%
  group_by(area_cat,conn_cat) %>%
  summarize(med = median(sort(metric)[-1])) 

medians2 = plotdf %>%
  filter(!c(origin_ID%in%rm_polys)) %>%
  st_drop_geometry() %>%
  dplyr::select(-geometry.y) %>%
  left_join(.,plotdf %>%
              st_drop_geometry() %>%
              dplyr::select(-geometry.y)) %>%
  group_by(area_cat,conn_cat) %>%
  summarize(med = median(metric)) 

pdf2=plotdf %>%
  filter(!c(origin_ID%in%rm_polys)) %>%
  st_drop_geometry() %>%
  dplyr::select(-geometry.y) %>%
  left_join(.,medians) %>%
  group_by(area_cat,conn_cat) %>%
  filter(metric==med)
pdf3 = plotdf %>%
  filter(!c(origin_ID%in%rm_polys)) %>%
  st_drop_geometry() %>%
  dplyr::select(-geometry.y) %>%
  left_join(.,medians2) %>%
  group_by(area_cat,conn_cat) %>%
  filter(metric==med)
fin_pdf = rbind(pdf2,pdf3) %>%
  arrange(conn_cat,area_cat)



selected_polys = c(fin_pdf[1:9,1])
selected_polys = unlist(selected_polys)
selected_polys = c(8145,4892,5501,1818,861,2633,5939,823,70)
# selected_polys = c(9045,
#                    7696,
#                    3556,# High Area, Medium Connectivity
#                    5869,
#                    354,
#                    236,
#                    1930,
#                    2866,
#                    133)# High Area, High Connectivity

titledf = c("Low connectivity, small patch",
            "Low connectivity, medium patch",
            "Low connectivity, large patch",
            "Medium connectivity, small patch",
            "Medmium connectivity, medium patch",
            "Medium connectivity, large patch",
            "High connectivity, small patch",
            "High connectivity, medium patch",
            "High connectivity, large patch")
sindf3 = sinuosity_df %>%
  dplyr::mutate(origin_ID = layer,
         destination_ID = layer.1)
#####
# Use to find polygons
#####

ind=0
for(i in 401:500){
  ind=ind+1
  sp = fin_poly[i,] %>% 
    mutate(facet_id = ind)
  sl = line_df %>%
    filter(origin_ID==sp$layer) 
  sp2 = fin_poly %>%
    filter(layer %in% sl$destination_ID) %>%
    mutate(facet_id = ind)
  if(ind==1){
    sp3 = sp
    sp4 = sp2
  }
  if(ind>1){
    sp3 = rbind(sp,sp3)
    sp4 = rbind(sp2,sp4)
  }
}
tmap_mode('view')
m=tm_shape(sp4)+
  tm_polygons()+
  tm_facets(by='facet_id')+
  tm_shape(sp3)+
  tm_polygons(col='red')+
  tm_facets(by='facet_id')+
  tm_shape(sp3)+
  tm_text(text = 'layer')+
  tm_facets(by='facet_id')
#tmap_save(m,filename=paste0(getwd(),'/scrap.jpeg'))
tmap_mode('view')
tm_shape(fin_poly,bbox = fin_poly %>% filter(layer == 5939) %>%
             st_bbox())+
  tm_polygons()+
tm_shape(fin_poly %>%
           filter(layer == 5939))+
  tm_polygons(col='red')

  
#####
# Build map
#####
ind=0
for(i in unique(selected_polys)){
  ind=ind+1
  lcp_network2 = sindf3 %>%
    dplyr::filter(origin_ID==i|
             destination_ID==i) %>%
    #st_drop_geometry() %>%
    #left_join(.,lcp_network,by=c('origin_ID','destination_ID')) %>%
    st_set_geometry('geometry')
    #filter(sf::st_is_empty(.)==F)
  nodelist = line_df %>% filter(origin_ID==selected_polys[ind])
  nodelist2 = line_df %>% filter(destination_ID==selected_polys[ind]) 
  nodelistfin = c(nodelist$destination_ID,nodelist$origin_ID,nodelist2$destination_ID,nodelist2$origin_ID)
  text = merged_df %>% st_drop_geometry() %>%
    filter(layer==selected_polys[ind]) %>%
    dplyr::select(metric)
  MM = merged_df %>% st_drop_geometry() %>%
    filter(layer==selected_polys[ind]) #%>%
    #dplyr::select(small_metric)
  bb1 = merged_df %>% 
    filter(layer %in% c(nodelistfin)) %>%
    st_union() %>%
    tmaptools::bb_poly() %>%
    st_area()
  bb1 = set_units(bb1,ha)
  attributes(bb1)=NULL
if(!ind%in%c(4,5,6,8)){    
  map = tm_shape(merged_df %>% filter(layer %in% c(nodelistfin))) +
    tm_polygons(alpha=.45,col='grey80')+#col='i_area',
    # breaks=c(1000000,2000000,3000000,4000000,
    #          5000000,7500000,20000000))+
    #tm_borders(alpha=.45,col='black')+
    tm_shape(st_centroid(merged_df %>% filter(layer %in% c(nodelistfin))))+
    tm_dots(size=.05)+
    #tm_shape(line_df %>% 
    #           filter(origin_ID==i|
    #                    destination_ID==i))+
    #tm_lines(col='blue',lwd=2)+
    tm_shape(lcp_network2)+
    tm_lines(col='black',lwd=3)+
    tm_shape(lcp_network2 %>% arrange(inv_sinuosity))+
    tm_lines(col='inv_sinuosity',
             legend.col.show = F,
             lwd=2,
             breaks=c(.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95,1),
             palette = get_brewer_pal("Blues",n=13))+
    tm_shape(merged_df %>% filter(layer==selected_polys[ind]))+
    tm_polygons(col="red",
                border.col = 'black')+
    tm_scale_bar(position=c(.6,.9))+
    #tm_add_legend(title = paste0(round(bb1)," ha"))+
    tm_credits(text=paste0("SCR = ",round(text,4)),
               size=1,position = c('center','BOTTOM'))+
    tm_layout(#legend.position=c(.7,.85),
              main.title.size = .75,
              attr.outside = F,
              attr.outside.position = 'bottom',
              main.title = titledf[ind])}
if(ind%in%c(4,5)){map = tm_shape(merged_df %>% filter(layer %in% c(nodelistfin))) +
    tm_polygons(alpha=.45,col='grey80')+#col='i_area',
    # breaks=c(1000000,2000000,3000000,4000000,
    #          5000000,7500000,20000000))+
    #tm_borders(alpha=.45,col='black')+
    tm_shape(st_centroid(merged_df %>% filter(layer %in% c(nodelistfin))))+
    tm_dots(size=.05)+
    #tm_shape(line_df %>% 
    #           filter(origin_ID==i|
    #                    destination_ID==i))+
    #tm_lines(col='blue',lwd=2)+
    tm_shape(lcp_network2)+
    tm_lines(col='black',lwd=3)+
    tm_shape(lcp_network2 %>% arrange(inv_sinuosity))+
    tm_lines(col='inv_sinuosity',
             legend.col.show = F,
             lwd=2,
             breaks=c(.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95,1),
             palette = get_brewer_pal("Blues",n=13))+
    tm_shape(merged_df %>% filter(layer==selected_polys[ind]))+
    tm_polygons(col="red",
                border.col = 'black')+
  tm_scale_bar(position=c(.05,.875))+
    #tm_add_legend(title = paste0(round(bb1)," ha"))+
    tm_credits(text=paste0("SCR = ",round(text,4)),
               size=1,position = c('center','BOTTOM'))+
    tm_layout(#legend.position=c(.72,.85),
              main.title.size = .75,
              attr.outside = F,
              attr.outside.position = 'bottom',
              main.title = titledf[ind])}
if(ind==6){    
    map = tm_shape(merged_df %>% filter(layer %in% c(nodelistfin))) +
      tm_polygons(alpha=.45,col='grey80')+#col='i_area',
      # breaks=c(1000000,2000000,3000000,4000000,
      #          5000000,7500000,20000000))+
      #tm_borders(alpha=.45,col='black')+
      tm_shape(st_centroid(merged_df %>% filter(layer %in% c(nodelistfin))))+
      tm_dots(size=.05)+
      #tm_shape(line_df %>% 
      #           filter(origin_ID==i|
      #                    destination_ID==i))+
      #tm_lines(col='blue',lwd=2)+
      tm_shape(lcp_network2)+
      tm_lines(col='black',lwd=3)+
      tm_shape(lcp_network2 %>% arrange(inv_sinuosity))+
      tm_lines(col='inv_sinuosity',
               legend.col.show = F,
               lwd=2,
               breaks=c(.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95,1),
               palette = get_brewer_pal("Blues",n=13))+
      tm_shape(merged_df %>% filter(layer==selected_polys[ind]))+
      tm_polygons(col="red",
                  border.col = 'black')+
      tm_scale_bar(position=c(.3,.875))+
      #tm_add_legend(title = paste0(round(bb1)," ha"))+
      tm_credits(text=paste0("SCR = ",round(text,4)),
                 size=1,position = c('center','BOTTOM'))+
      tm_layout(#legend.position=c(.35,.85),
                main.title.size = .75,
                attr.outside = F,
                attr.outside.position = 'bottom',
                main.title = titledf[ind])}
if(ind==8){map = tm_shape(merged_df %>% filter(layer %in% c(nodelistfin))) +
    tm_polygons(alpha=.45,col='grey80')+#col='i_area',
    # breaks=c(1000000,2000000,3000000,4000000,
    #          5000000,7500000,20000000))+
    #tm_borders(alpha=.45,col='black')+
    tm_shape(st_centroid(merged_df %>% filter(layer %in% c(nodelistfin))))+
    tm_dots(size=.05)+
    #tm_shape(line_df %>% 
    #           filter(origin_ID==i|
    #                    destination_ID==i))+
    #tm_lines(col='blue',lwd=2)+
    tm_shape(lcp_network2)+
    tm_lines(col='black',lwd=3)+
    tm_shape(lcp_network2 %>% arrange(inv_sinuosity))+
    tm_lines(col='inv_sinuosity',
             legend.col.show = F,
             lwd=2,
             breaks=c(.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95,1),
             palette = get_brewer_pal("Blues",n=13))+
    tm_shape(merged_df %>% filter(layer==selected_polys[ind]))+
    tm_polygons(col="red",
                border.col = 'black')+
  tm_scale_bar(position=c(.625,.85))+
   # tm_add_legend(title = paste0(round(bb1)," ha"))+
    tm_credits(text=paste0("SCR = ",round(text,4)),
               size=1,position = c('center','BOTTOM'))+
    tm_layout(#legend.position=c(.675,.85),
              main.title.size = .75,
              attr.outside = F,
              attr.outside.position = 'bottom',
              main.title = titledf[ind])}
  
  assign(paste0("map_",ind),map)    
}

full_map=tmap_arrange(map_1,map_2,map_3,
                      map_4,map_5,map_6,
                      map_7,map_8,map_9,
                      ncol=3)
#full_map

tmap_save(full_map,
          filename=paste0(getwd(),"/Figures/3_3_example_scalebar.jpeg"),dpi=300)

