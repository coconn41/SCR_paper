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
fin_poly2 = fin_poly %>%
  filter(area>mean(fin_poly$area))
fin_poly3 = fin_poly %>%
  filter(area<=mean(area,na.rm=T)&
           area>median(area,na.rm=T))
set.seed(314)
fin_poly4 = fin_poly3[sample(nrow(fin_poly3),nrow(fin_poly3)*.05),]
fin_poly = rbind(fin_poly2,fin_poly4)
remove(fin_poly2)
wmu_nodes = st_centroid(fin_poly)