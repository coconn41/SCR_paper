library(sf)
library(dplyr)
library(readxl)
library(tmap)
library(FedData)
library(terra)
library(units)
library(landscapemetrics)
library(foreach)
library(igraph)
tdir=tempdir()

countyurl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_500k.zip"

if(file.exists(paste(tdir,"/cb_2018_us_county_500k.shp",sep=""))==F){
  download.file(countyurl,destfile = file.path(tdir,"Counties.zip"))
  unzip(file.path(tdir,"Counties.zip"),exdir=tdir)}

USA_Counties=read_sf(paste(tdir,"/cb_2018_us_county_500k.shp",sep=""))

Manhattan = USA_Counties %>%
  filter(NAME=="New York") %>%
  st_transform(.,32618)

CP_points = data.frame(id = c(1,1,1,1),
                       X = c(-73.981641,
                             -73.973052,
                             -73.949363,
                             -73.958107),
                       Y = c(40.768152,
                             40.764478,
                             40.796848,
                             40.800464)) %>%
  st_as_sf(coords = c('X','Y'),crs=4326)

Central_park = CP_points %>%
  group_by(id) %>%
  summarize() %>%
  st_cast('POLYGON')


##### 
# Do for central park
#####

forests=get_nlcd(template = Central_park,
                 year = 2019,
                 label = "Forests")

LCr = rast(forests)
Central_park = st_transform(Central_park,crs(LCr))
LCproj = terra::project(LCr,crs(Central_park))

LCcrop = terra::crop(x = LCproj,
                     y = Central_park %>%
                       terra::vect(),
                     mask = T)

LC_forest_patches = LCcrop
values(LC_forest_patches)[values(LC_forest_patches)==42] = 41
values(LC_forest_patches)[values(LC_forest_patches)==43] = 41
values(LC_forest_patches)[values(LC_forest_patches)!=41] = NA

y = get_patches(LC_forest_patches,directions=4)#patches(LC_forest_patches,directions=4)
poly1 = as.polygons(terra::rast(y$layer_1$class_41))
poly2 = st_as_sf(poly1)
poly2$area = st_area(poly2)
poly2$area = set_units(poly2$area,ha)
attributes(poly2$area)=NULL
fin_poly = poly2
wmu_nodes = st_centroid(fin_poly)
cp_fin_poly = fin_poly

# SCR
cores = 4
distance = 1675
Central_park$area = st_area(Central_park);Central_park$area
Central_park$area = set_units(Central_park$area,ha);Central_park$area
attributes(Central_park$area)=NULL
template = Central_park
wmus = Central_park
sing_wmu = Central_park
source(paste0(getwd(),"/Scripts/SCR/Roadway_data_cleaning.R"))
source(paste0(getwd(),"/Scripts/SCR/Alpine_ecozone_cleaning.R"))
source(paste0(getwd(),"/Scripts/Universal/Process_nodes.R"))
source(paste0(getwd(),"/Scripts/SCR/Resistance_raster_processing.R"))
source(paste0(getwd(),"/Scripts/SCR/Within_distance_comps.R"))
source(paste0(getwd(),"/Scripts/SCR/Landscape_network.R"))
source(paste0(getwd(),"/Scripts/SCR/Get_euclidean_distances.R"))
names(sinuosity_df)[1]="layer"
names(sinuosity_df)[2]="layer.1"

combdf = left_join(fin_poly,
                   sinuosity_df,
                   by=c('layer')) %>%
  left_join(.,fin_poly %>% st_drop_geometry(),
            by=c('layer.1'='layer')) %>%
  rename('i_area' = 'area.x',
         'j_area' = 'area.y')

final_metric_CP = combdf %>%
  mutate(sijaj = inv_sinuosity*j_area) %>%
  st_drop_geometry() %>%
  dplyr::select(-c(geometry.x,geometry.y)) %>%
  group_by(layer) %>%
  summarize(sijaj_sum = sum(sijaj,na.rm=T)) %>%
  ungroup() %>%
  left_join(.,fin_poly %>%
              st_drop_geometry(),
            by='layer') %>%
  mutate(numerator = sijaj_sum+area) %>%
  summarize(numerator_sum = sum(sijaj_sum,na.rm=T))

metric_fin_CP = data.frame(UNIT = "Central Park",
                                  wmu_type = "normal",
                                  index = "SCR",
                                  value = as.numeric(final_metric_CP)/(nrow(fin_poly)*sing_wmu$area)) 

#

comb_mat = matrix(nrow=nrow(wmu_nodes),
                  ncol = nrow(wmu_nodes))  
combinations <- which(upper.tri(comb_mat,
                                diag = FALSE),
                      arr.ind = TRUE)
combinations=as.data.frame(cbind(combinations, comb_mat[combinations]))
names(combinations)=c('i','j','Nas')
combinations = combinations %>%
  arrange(i) %>%
  dplyr::select(-Nas)


myCluster <- parallel::makeCluster(4)
doParallel::registerDoParallel(myCluster)
numerator <- foreach::foreach(a = 1:nrow(combinations),
                              .verbose = F,
                              .errorhandling = "remove",
                              .combine = "rbind",
                              .packages = c("sf",
                                            "raster",
                                            "dplyr",
                                            "terra",
                                            "igraph",
                                            "units")) %dopar% {
                                              ipatch = wmu_nodes[combinations[a,1],]
                                              jpatch = wmu_nodes[combinations[a,2],]
                                              combodf = data.frame(i = combinations[a,1],
                                                                   j = combinations[a,2],
                                                                   i_area = ipatch$area,
                                                                   j_area = jpatch$area,
                                                                   distance = st_distance(ipatch,jpatch))
                                              return(combodf)
                                            }
parallel::stopCluster(myCluster)

attributes(numerator$distance)=NULL
numerator$product_prob=exp(-.001788497*numerator$distance)
numerator$max_distance = NA



g=igraph::graph_from_data_frame(numerator,directed=TRUE,vertices = NULL)
E(g)$weight = 1/(numerator$product_prob)
myCluster <- parallel::makeCluster(4)
doParallel::registerDoParallel(myCluster)
full_max_df <- foreach::foreach(a = 1:nrow(wmu_nodes),
                                .verbose = TRUE,
                                .errorhandling = "remove",
                                .combine = "rbind",
                                .packages = c("sf",
                                              "raster",
                                              "dplyr",
                                              "terra",
                                              "igraph",
                                              "units")) %dopar% {
                                                i_ind=0
                                                list_i = shortest_paths(g,from=V(g)[a],to=V(g))
                                                for(b in 1:as.numeric(length(list_i$vpath))){
                                                  plength = length(list_i$vpath[[b]])
                                                  if(plength<=1){next}
                                                  i_ind=i_ind+1
                                                  for(c in 1:plength){
                                                    if(c==plength){break}
                                                    if(c==1){combos = data.frame(i=as.numeric(list_i$vpath[[b]][c]),
                                                                                 j=as.numeric(list_i$vpath[[b]][c+1]))}
                                                    if(c>1){combos2 = data.frame(i=as.numeric(list_i$vpath[[b]][c]),
                                                                                 j=as.numeric(list_i$vpath[[b]][c+1]))
                                                    combos = rbind(combos2,combos)}
                                                  }
                                                  if(i_ind==1){df=left_join(as.data.frame(combos),
                                                                            as.data.frame(numerator),
                                                                            join_by(i,j)) %>%
                                                    summarize(max_distance=sum(distance)) %>%
                                                    as.data.frame()
                                                  df$i = as.numeric(list_i$vpath[[b]][1])
                                                  df$j = as.numeric(list_i$vpath[[b]][plength])}
                                                  if(i_ind>1){df2=left_join(as.data.frame(combos),
                                                                            as.data.frame(numerator),
                                                                            join_by(i,j)) %>%
                                                    summarize(max_distance=sum(distance)) %>%
                                                    as.data.frame()
                                                  df2$i = as.numeric(list_i$vpath[[b]][1])
                                                  df2$j = as.numeric(list_i$vpath[[b]][plength])
                                                  df=rbind(df2,df)}
                                                }
                                                return(df)
                                              }
parallel::stopCluster(myCluster)

numerator = as.data.frame(numerator) %>%
  select(-max_distance) %>%
  left_join(.,
            as.data.frame(full_max_df),
            join_by(i,j)) %>%
  distinct()
numerator$product_prob = exp(-.001788497*numerator$max_distance)
numerator$act_numerator = numerator$i_area*numerator$j_area*numerator$product_prob

CP_Area = st_area(Central_park)
CP_Area = set_units(CP_Area,ha)
attributes(CP_Area)=NULL

result_df = data.frame(UNIT = rep("Central_park",4),
                       wmu_type = rep("normal",4),
                       index = c("PC_index_half",
                                 "PC_index_double",
                                 "ECA_half",
                                 "ECA_double"),
                       value = c(sum(numerator$act_numerator)/(CP_Area^2),
                                 sum(numerator$act_numerator*2)/(CP_Area^2),
                                 sqrt(sum(numerator$act_numerator)),
                                 sqrt(sum(numerator$act_numerator*2))))

Manhattan_Area = st_area(Manhattan)
Manhattan_Area = set_units(Manhattan_Area,ha)
attributes(Manhattan_Area)=NULL

size_change_result = data.frame(UNIT = rep("Increased_size",4),
                                wmu_type = rep("normal",4),
                                index = c("PC_index_half",
                                          "PC_index_double",
                                          "ECA_half",
                                          "ECA_double"),
                                value = c(sum(numerator$act_numerator)/(Manhattan_Area^2),
                                          sum(numerator$act_numerator*2)/(Manhattan_Area^2),
                                          sqrt(sum(numerator$act_numerator)),
                                          sqrt(sum(numerator$act_numerator*2))))
metric_fin_increased_size = data.frame(UNIT = "Increased_size",
                           wmu_type = "normal",
                           index = "SCR",
                           value = as.numeric(final_metric_CP)/(nrow(fin_poly)*Manhattan_Area)) 

#####
# Do for all of Manhattan
#####

forests=get_nlcd(template = Manhattan,
                 year = 2019,
                 label = "Forests",force.redo = T)

LCr = rast(forests)
Manhattan = st_transform(Manhattan,crs(LCr))
LCproj = terra::project(LCr,crs(Manhattan))

LCcrop = terra::crop(x = LCproj,
                     y = Manhattan %>%
                       terra::vect(),
                     mask = T)

LC_forest_patches = LCcrop
values(LC_forest_patches)[values(LC_forest_patches)==42] = 41
values(LC_forest_patches)[values(LC_forest_patches)==43] = 41
values(LC_forest_patches)[values(LC_forest_patches)!=41] = NA

y = get_patches(LC_forest_patches,directions=4)#patches(LC_forest_patches,directions=4)
poly1 = as.polygons(terra::rast(y$layer_1$class_41))
poly2 = st_as_sf(poly1)
poly2$area = st_area(poly2)
poly2$area = set_units(poly2$area,ha)
attributes(poly2$area)=NULL
fin_poly = poly2
wmu_nodes = st_centroid(fin_poly)
Man_fin_poly = fin_poly
# SCR
cores = 4
distance = 1675
Manhattan$area = st_area(Manhattan)
Manhattan$area = set_units(Manhattan$area,ha)
attributes(Manhattan$area)=NULL
template = Manhattan
wmus = Manhattan
sing_wmu = Manhattan
source(paste0(getwd(),"/Scripts/SCR/Roadway_data_cleaning.R"))
source(paste0(getwd(),"/Scripts/SCR/Alpine_ecozone_cleaning.R"))
source(paste0(getwd(),"/Scripts/Universal/Process_nodes.R"))
source(paste0(getwd(),"/Scripts/SCR/Resistance_raster_processing.R"))
source(paste0(getwd(),"/Scripts/SCR/Within_distance_comps.R"))
source(paste0(getwd(),"/Scripts/SCR/Landscape_network.R"))
source(paste0(getwd(),"/Scripts/SCR/Get_euclidean_distances.R"))
names(sinuosity_df)[1]="layer"
names(sinuosity_df)[2]="layer.1"

combdf = left_join(fin_poly,
                   sinuosity_df,
                   by=c('layer')) %>%
  left_join(.,fin_poly %>% st_drop_geometry(),
            by=c('layer.1'='layer')) %>%
  rename('i_area' = 'area.x',
         'j_area' = 'area.y')

final_metric = combdf %>%
  mutate(sijaj = inv_sinuosity*j_area) %>%
  st_drop_geometry() %>%
  dplyr::select(-c(geometry.x,geometry.y)) %>%
  group_by(layer) %>%
  summarize(sijaj_sum = sum(sijaj,na.rm=T)) %>%
  ungroup() %>%
  left_join(.,fin_poly %>%
              st_drop_geometry(),
            by='layer') %>%
  mutate(numerator = sijaj_sum+area) %>%
  summarize(numerator_sum = sum(sijaj_sum,na.rm=T))

metric_fin_Manhattan = data.frame(UNIT = "Manhattan",
                                  wmu_type = "normal",
                                  index = "SCR",
                                  value = as.numeric(final_metric)/(nrow(fin_poly)*sing_wmu$area)) 


comb_mat = matrix(nrow=nrow(wmu_nodes),
                  ncol = nrow(wmu_nodes))  
combinations <- which(upper.tri(comb_mat,
                                diag = FALSE),
                      arr.ind = TRUE)
combinations=as.data.frame(cbind(combinations, comb_mat[combinations]))
names(combinations)=c('i','j','Nas')
combinations = combinations %>%
  arrange(i) %>%
  dplyr::select(-Nas)


myCluster <- parallel::makeCluster(4)
doParallel::registerDoParallel(myCluster)
numerator <- foreach::foreach(a = 1:nrow(combinations),
                              .verbose = F,
                              .errorhandling = "remove",
                              .combine = "rbind",
                              .packages = c("sf",
                                            "raster",
                                            "dplyr",
                                            "terra",
                                            "igraph",
                                            "units")) %dopar% {
                                              ipatch = wmu_nodes[combinations[a,1],]
                                              jpatch = wmu_nodes[combinations[a,2],]
                                              combodf = data.frame(i = combinations[a,1],
                                                                   j = combinations[a,2],
                                                                   i_area = ipatch$area,
                                                                   j_area = jpatch$area,
                                                                   distance = st_distance(ipatch,jpatch))
                                              return(combodf)
                                            }
parallel::stopCluster(myCluster)

attributes(numerator$distance)=NULL
numerator$product_prob=exp(-.001788497*numerator$distance)
numerator$max_distance = NA



g=igraph::graph_from_data_frame(numerator,directed=TRUE,vertices = NULL)
E(g)$weight = 1/(numerator$product_prob)
myCluster <- parallel::makeCluster(4)
doParallel::registerDoParallel(myCluster)
full_max_df <- foreach::foreach(a = 1:nrow(wmu_nodes),
                                .verbose = TRUE,
                                .errorhandling = "remove",
                                .combine = "rbind",
                                .packages = c("sf",
                                              "raster",
                                              "dplyr",
                                              "terra",
                                              "igraph",
                                              "units")) %dopar% {
                                                i_ind=0
                                                list_i = shortest_paths(g,from=V(g)[a],to=V(g))
                                                for(b in 1:as.numeric(length(list_i$vpath))){
                                                  plength = length(list_i$vpath[[b]])
                                                  if(plength<=1){next}
                                                  i_ind=i_ind+1
                                                  for(c in 1:plength){
                                                    if(c==plength){break}
                                                    if(c==1){combos = data.frame(i=as.numeric(list_i$vpath[[b]][c]),
                                                                                 j=as.numeric(list_i$vpath[[b]][c+1]))}
                                                    if(c>1){combos2 = data.frame(i=as.numeric(list_i$vpath[[b]][c]),
                                                                                 j=as.numeric(list_i$vpath[[b]][c+1]))
                                                    combos = rbind(combos2,combos)}
                                                  }
                                                  if(i_ind==1){df=left_join(as.data.frame(combos),
                                                                            as.data.frame(numerator),
                                                                            join_by(i,j)) %>%
                                                    summarize(max_distance=sum(distance)) %>%
                                                    as.data.frame()
                                                  df$i = as.numeric(list_i$vpath[[b]][1])
                                                  df$j = as.numeric(list_i$vpath[[b]][plength])}
                                                  if(i_ind>1){df2=left_join(as.data.frame(combos),
                                                                            as.data.frame(numerator),
                                                                            join_by(i,j)) %>%
                                                    summarize(max_distance=sum(distance)) %>%
                                                    as.data.frame()
                                                  df2$i = as.numeric(list_i$vpath[[b]][1])
                                                  df2$j = as.numeric(list_i$vpath[[b]][plength])
                                                  df=rbind(df2,df)}
                                                }
                                                return(df)
                                              }
parallel::stopCluster(myCluster)

numerator = as.data.frame(numerator) %>%
  select(-max_distance) %>%
  left_join(.,
            as.data.frame(full_max_df),
            join_by(i,j)) %>%
  distinct()
numerator$product_prob = exp(-.001788497*numerator$max_distance)
numerator$act_numerator = numerator$i_area*numerator$j_area*numerator$product_prob

result_df2 = data.frame(UNIT = rep("Manhattan",4),
                        wmu_type = rep("normal",4),
                        index = c("PC_index_half",
                                  "PC_index_double",
                                  "ECA_half",
                                  "ECA_double"),
                        value = c(sum(numerator$act_numerator)/(Manhattan_Area^2),
                                  sum(numerator$act_numerator*2)/(Manhattan_Area^2),
                                  sqrt(sum(numerator$act_numerator)),
                                  sqrt(sum(numerator$act_numerator*2))))



full_results = rbind(result_df,result_df2) %>%
  rbind(.,size_change_result) %>%
  rbind(.,metric_fin_CP) %>%
  rbind(.,metric_fin_Manhattan) %>%
  rbind(.,metric_fin_increased_size)

m1=tm_shape(Central_park)+tm_borders()+tm_shape(cp_fin_poly)+tm_polygons(col='forestgreen')+
  tm_credits(text = "PC = 4.94e-4\nSCR = 1.28e-2\nECA = 7.636",
             size=1,position=c('left','top'))+
  tm_scale_bar(position=c('right','bottom'),text.size = 1);m1
#tm_scale_bar(position=c('right','bottom'),
#             text.size=1,widbreaks = NULL)

m2 = tm_shape(Manhattan)+tm_borders()+
  tm_shape(Central_park)+
  tm_borders(alpha=.5)+
  tm_shape(cp_fin_poly)+
  tm_polygons(col='forestgreen')+
  tm_credits(text = "PC = 8.47e-7\nSCR = 5.30e-4\nECA = 7.636",
             size = 1,position = c('left','top'))+
  tm_scale_bar(position=c('right','bottom'),text.size = 1);m2

full_map = tmap_arrange(m1,m2)

tmap_save(full_map,paste0(getwd(),"/Figures/Relative_distance.jpeg"),
          dpi=300)

tmap_save(full_map,paste0(getwd(),"/Figures/Relative_distance.tiff"),
          dpi=300)
