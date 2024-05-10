combdf = fin_poly[comps2$row,] %>%
  st_drop_geometry() %>%
  bind_cols(.,fin_poly[comps2$col,] %>%
              st_drop_geometry())
names(combdf)=c("origin_ID","i_area","destination_ID","j_area")

sindf_UTLT = sinuosity_df %>%
  st_drop_geometry() %>%
  bind_rows(.,sinuosity_df %>%
              st_drop_geometry() %>%
              mutate(origin_ID2 = destination_ID,
                     destination_ID2 = origin_ID) %>%
              select(-c(destination_ID,origin_ID)) %>%
              mutate(origin_ID = origin_ID2,
                     destination_ID = destination_ID2) %>%
              select(-c(destination_ID2,origin_ID2))) %>%
  arrange(origin_ID,destination_ID)

combdf = combdf %>%
  left_join(.,
            sindf_UTLT,
            by=c('origin_ID','destination_ID'))

final_metric = combdf %>%
  mutate(sijaj = inv_sinuosity*j_area) %>%
  group_by(origin_ID) %>%
  summarize(sijaj_sum = sum(sijaj,na.rm=T)) %>%
  ungroup() %>%
  left_join(.,fin_poly %>%
              st_drop_geometry(),
            by=c('origin_ID'='layer')) %>%
  mutate(numerator = sijaj_sum+area) %>%
  summarize(numerator_sum = sum(sijaj_sum,na.rm=T))

metric_fin = final_metric/(nrow(fin_poly)*sing_wmu$area);metric_fin

wmu_df = data.frame(UNIT = sing_wmu$UNIT,
                    metric = "custom_LS_metric",
                    value = as.numeric(metric_fin))
if(buff_unbuff=="Buffered"){write.csv(wmu_df,paste0(getwd(),'/Data/SCR/Landscape/Buffered/Landscape_results_Buffered_',
                        sing_wmu$UNIT,'.csv'))}
if(buff_unbuff=="Unbuffered"){write.csv(wmu_df,paste0(getwd(),'/Data/SCR/Landscape/Unbuffered/Landscape_results_Unbuffered_',
                                                    sing_wmu$UNIT,'.csv'))}