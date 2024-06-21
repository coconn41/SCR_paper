library(tmap)
library(tidyverse)
library(sf)
library(tmaptools)

tdir=tempdir()

for(i in 1:13){
  metric2 = read_sf(paste0(getwd(),'/Data/Output_data/SCR/Patch/Metric_results',i,'.shp'))
if(i==1){metric = read_sf(paste0(getwd(),'/Data/Output_data/SCR/Patch/Metric_results',i,'.shp'))}
if(i>1){metric = rbind(metric,metric2)}
}
remove(metric2)

stateurl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_500k.zip"
if(file.exists(paste(tdir,"/cb_2018_us_state_500k.shp",sep=""))==F){
  download.file(stateurl, destfile = file.path(tdir, "States.zip"))
  unzip(file.path(tdir,"States.zip"),exdir=tdir)}

NYS = read_sf(paste(tdir,"/cb_2018_us_state_500k.shp",sep="")) %>%
  filter(NAME=="New York") %>%
  st_transform(.,crs=32618)

#####
# Polygon map, very large shapefile, will hang.
#####

# m1=tm_shape(NYS)+
#   tm_borders()+
#   tm_shape(metric)+
#   tm_polygons(col='metric',
#               border.alpha = 0,
#               title = "Sinuous Connection Reduction",
#               palette = get_brewer_pal("Greens"),
#               style='cont')+
#   tm_layout(legend.position=c('left','top'),
#             main.title='A')

m2=tm_shape(NYS %>%
              st_buffer(.,dist=4000))+
  tm_borders()+
  tm_shape(metric %>%
             st_centroid() %>%
             arrange(metric))+
  tm_dots(col='metric',
          title = "Sinuous Connection Reduction",
          palette = get_brewer_pal("Greens"),
          style='cont')+
  tm_layout(legend.position=c('left','top'))
      #      main.title = "B") Include if doing a side by side map

tmap_save(m2,
          filename = paste0(getwd(),'/Figures/Patch_level_map_A.jpeg'),
          dpi=300)

#m3=tmap_arrange(m1,m2,ncol=2)

# tmap_save(m3,filename=paste0(getwd(),"/Figures/Patch_level_map.jpeg"),
#           dpi=300,
#           height=4,
#           width=7)
