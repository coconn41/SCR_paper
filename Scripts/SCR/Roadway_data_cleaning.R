minor_roadways = c(7,8,9,17,18,19)
major_roadways = c(1,2,4,6,11,12,14,16)

roadwaydat = read_sf(paste0(getwd(),
                            "/LCP_analysis/Input_data/NYSDOT Functional Class 07_10_2023/NYSDOT_Functional_Class_07_10_2023.shp")) %>%
  st_zm() %>%
  mutate(Categorization = ifelse(FUNCTIONAL %in% minor_roadways,1,
                                 ifelse(FUNCTIONAL %in% major_roadways,2,0))) %>%
  st_transform(.,crs=st_crs(NYS))

road_vect = terra::vect(roadwaydat)
road_pix  = terra::rasterize(x = road_vect,
                             y = LCcrop,
                             field = "Categorization")
