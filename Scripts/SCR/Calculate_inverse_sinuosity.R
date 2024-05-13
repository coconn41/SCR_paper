sinuosity_df = line_df %>%
  st_drop_geometry() %>%
  left_join(.,lcp_network %>% 
              st_drop_geometry(),by=c("origin_ID","destination_ID")) %>%
  #dplyr::select(-direction),by=c("origin_ID","destination_ID")) %>%
  mutate(length = ifelse(is.na(length)==T,0,length),
         sinuosity = length/euclid_lengths,
         inv_sinuosity = ifelse(length==0,0,euclid_lengths/length)) %>%
  left_join(.,fin_poly,by=c("destination_ID"="layer")) %>%
  #mutate(mini_metric = inv_sinuosity*area) %>%
  dplyr::select(-area)
