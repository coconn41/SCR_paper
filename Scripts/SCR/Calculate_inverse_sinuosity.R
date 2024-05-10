sinuosity_df = line_df %>%
  st_drop_geometry() %>%
  left_join(.,lcp_network %>% 
              filter(direction!="B to A") %>%
              st_drop_geometry() %>%
              dplyr::select(-direction),by=c("origin_ID","destination_ID")) %>%
  mutate(lengths = ifelse(is.na(length)==T,0,length),
         sinuosity = length/euclid_lengths,
         inv_sinuosity = ifelse(length==0,0,euclid_lengths/length)) %>%
  dplyr::select(-origin_ID) %>%
  group_by(destination_ID) %>%
  summarize(mean_sinuosity = mean(sinuosity),
            mean_inv_sinuosity = mean(inv_sinuosity))