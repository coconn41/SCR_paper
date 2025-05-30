names(sinuosity_df)[1]="layer"
names(sinuosity_df)[2]="layer.1"

combdf = left_join(clipped_polys %>% st_drop_geometry(),
                   sinuosity_df %>% st_drop_geometry()) %>%
  left_join(.,buffer_polys %>%
              st_drop_geometry() %>%
              dplyr::select(layer,i_area,di_area)) %>%
  filter(is.na(inv_sinuosity)==F)

merged_df = combdf %>%
  mutate(sin_area = j_area*inv_sinuosity) %>%
  group_by(layer) %>%
  summarize(metric = (max(i_area)+sum(sin_area))/max(di_area)) %>%
  left_join(.,fin_poly)%>%#,by=c("origin_ID"="layer")) %>%
  st_set_geometry('geometry')

sf::write_sf(merged_df,
             dsn=paste0(getwd(),"/Data/Output_data/SCR/Patch/Metric_",substring(Sys.time(),1,10),".shp"))
write.csv(merged_df %>%
            st_drop_geometry(),
          filename = paste0(getwd(),"/Data/Output_data/SCR/Patch/Metric_",substring(Sys.time(),1,10),".csv"))