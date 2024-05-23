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

metric_fin = final_metric/(nrow(fin_poly)*sing_wmu$area);metric_fin

wmu_df = data.frame(UNIT = sing_wmu$UNIT,
                    metric = "custom_LS_metric",
                    value = as.numeric(metric_fin))
if(buff_unbuff=="Buffered"){write.csv(wmu_df,paste0(getwd(),'/Data/SCR/Landscape/Buffered/Landscape_results_Buffered_',
                        sing_wmu$UNIT,'.csv'))}
if(buff_unbuff=="Unbuffered"){write.csv(wmu_df,paste0(getwd(),'/Data/SCR/Landscape/Unbuffered/Landscape_results_Unbuffered_',
                                                    sing_wmu$UNIT,'.csv'))}