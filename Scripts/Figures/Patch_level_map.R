library(tmap)
library(tidyverse)
library(sf)
library(grid)
library(tmaptools)

tdir=tempdir()

for(i in 1:13){
  metric2 = read_sf(paste0(getwd(),'/Data/Output_data/SCR/Patch/Metric',i,'.shp'))
if(i==1){metric = read_sf(paste0(getwd(),'/Data/Output_data/SCR/Patch/Metric',i,'.shp'))}
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

adk_newbb=metric
old_bb=st_bbox(NYS)
new_bb = c(460000,4750000,640000,4980000)
names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
attr(new_bb, "class") = "bbox"
box = tmaptools::bb_poly(new_bb) %>% st_set_crs(32618)

cat_newbb=metric
new_bb2 = c(490000,4625000,590000,4690000)
names(new_bb2) = c("xmin", "ymin", "xmax", "ymax")
attr(new_bb2, "class") = "bbox"
box2 = tmaptools::bb_poly(new_bb2) %>% st_set_crs(32618)

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
  tm_shape(box)+
  tm_borders()+
  tm_shape(box2)+
  tm_borders()+
  tm_layout(legend.position=c(.01,.75))+
  tm_scale_bar(position=c('left','top'))

ADK_polys = st_intersection(metric,box)
Cat_polys = st_intersection(metric,box2)

inset1 = tm_shape(ADK_polys %>% st_simplify(.,dTolerance = 100))+
  tm_polygons(col='metric',
          title = "Sinuous Connection Reduction",
          palette = get_brewer_pal("Greens"),
          style='cont',
          border.alpha = 0,
          legend.show=F)+
  tm_shape(NYS %>%
             st_buffer(.,dist=4000))+
  tm_borders()

inset2 = tm_shape(Cat_polys %>% st_simplify(.,dTolerance = 100))+
  tm_polygons(col='metric',
              title = "Sinuous Connection Reduction",
              palette = get_brewer_pal("Greens"),
              style='cont',
              border.alpha = 0,
              legend.show=F)+
  tm_shape(NYS %>%
             st_buffer(.,dist=4000))+
  tm_borders()


vp1=viewport(x=.887,y=.74, width = .19)
vp2=viewport(x=.35,y=.175, width = .35)

vplist=list(vp1,vp2)
insetlist=list(inset1,inset2)

tmap_save(m2,
          insets_tm = insetlist,
          insets_vp = vplist,
          filename = paste0(getwd(),'/Figures/Patch_level_map_A_scalebar.jpeg'),
          dpi=300)
