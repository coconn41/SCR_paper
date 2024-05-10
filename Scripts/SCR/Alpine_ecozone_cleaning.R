alpine_regions = read_sf(paste0(getwd(),"/Data/Input_data/ny_eco_14/ny_eco_l4.shp")) %>% 
  filter(US_L4CODE=="58j") %>%
  mutate(US_L4CODE=1000) %>%
  st_transform(crs=32618)

alpine_pix = terra::rasterize(x = alpine_regions,
                              y = LCcrop,
                              field = "US_L4CODE",
                              background = 0)