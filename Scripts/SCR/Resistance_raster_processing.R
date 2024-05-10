LC_forest = LCcrop
forest_values = c(41,42,43,51,52,71)
values(LC_forest)[!(values(LC_forest)%in%forest_values)] = 0
values(LC_forest)[values(LC_forest)%in%forest_values] = 1

LC_cropland = LCcrop
cropland_values = c(81,82)
values(LC_cropland)[!(values(LC_cropland)%in%cropland_values)] = 0
values(LC_cropland)[values(LC_cropland)%in%cropland_values] = 35

LC_wetland = LCcrop
wetland_values = c(90,95)
values(LC_wetland)[!(values(LC_wetland)%in%wetland_values)] = 0
values(LC_wetland)[values(LC_wetland)%in%wetland_values] = 100

LC_water = LCcrop
water_values = c(11)
values(LC_water)[!(values(LC_water)%in%wetland_values)] = 0
values(LC_water)[values(LC_water)%in%wetland_values] = 1000

LC_high_developed = LCcrop
high_developed_values = c(24)
values(LC_high_developed)[!(values(LC_high_developed)%in%high_developed_values)] = 0
values(LC_high_developed)[values(LC_high_developed)%in%high_developed_values] = 1000

LC_med_developed = LCcrop
med_developed_values = c(23)
values(LC_med_developed)[!(values(LC_med_developed)%in%med_developed_values)] = 0
values(LC_med_developed)[values(LC_med_developed)%in%med_developed_values] = 100

LC_low_developed = LCcrop
low_developed_values = c(21,22)
values(LC_low_developed)[!(values(LC_low_developed)%in%low_developed_values)] = 0
values(LC_low_developed)[values(LC_low_developed)%in%low_developed_values] = 27

#Alpine is already classified

Highways = road_pix
highway_values = c(2)
values(Highways)[!(values(Highways)%in%highway_values)] = 0
values(Highways)[values(Highways)%in%highway_values] = 533

minor_roads = road_pix
minor_road_values = c(1)
values(minor_roads)[!(values(minor_roads)%in%minor_road_values)] = 0
values(minor_roads)[values(minor_roads)%in%minor_road_values] = 100

Resistance_grid = sum(LC_forest,LC_cropland,na.rm=T)
Resistance_grid = sum(Resistance_grid,LC_wetland,na.rm=T)
Resistance_grid = sum(Resistance_grid,LC_water,na.rm=T)
Resistance_grid = sum(Resistance_grid,LC_high_developed,na.rm=T)
Resistance_grid = sum(Resistance_grid,LC_med_developed,na.rm=T)
Resistance_grid = sum(Resistance_grid,LC_low_developed,na.rm=T)
Resistance_grid = sum(Resistance_grid,alpine_pix,na.rm=T)
Resistance_grid = sum(Resistance_grid,Highways,na.rm=T)
Resistance_grid = sum(Resistance_grid,minor_roads,na.rm=T)
Resistance_grid[Resistance_grid==0]=NA
Resistance_grid=1/Resistance_grid # These are now conductances, the inverse was taken
Rgrid = raster::raster(Resistance_grid) 