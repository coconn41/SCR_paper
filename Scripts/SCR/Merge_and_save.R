clipped_polys = clipped_polys %>%
  st_drop_geometry() %>%
  group_by(layer) %>%
  summarize(tot_area = sum(red_area)) %>%
  left_join(.,buffer_polys,by='layer') %>%
  mutate(perc_patch_cov = tot_area/area)


merged_df = left_join(clipped_polys %>%
                        st_drop_geometry(),sinuosity_df,by=c('layer'='destination_ID')) %>%
  mutate(mean_sinuosity = ifelse(is.na(mean_sinuosity)==T,0,mean_sinuosity),
         metric = ifelse(mean_sinuosity==0,0,perc_patch_cov * mean_inv_sinuosity)) %>%
  st_drop_geometry() %>%
  dplyr::select(layer,perc_patch_cov,mean_sinuosity,mean_inv_sinuosity,metric) %>%
  left_join(fin_poly,.,by='layer')

sf::write_sf(merged_df,
             dsn=paste0(getwd(),"/Data/Output_data/SCR/Patch/Metric_",substring(Sys.time(),1,10),".shp"))
write.csv(merged_df %>%
            st_drop_geometry(),
          filename = paste0(getwd(),"/Data/Output_data/SCR/Patch/Metric_",substring(Sys.time(),1,10),".csv"))