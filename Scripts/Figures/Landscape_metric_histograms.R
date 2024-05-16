#####
# Load regression/analysis datasets:
#####
source(paste0(getwd(),'/Scripts/Analysis/Stat_dataset_compile.R'))

#####
# Plot histograms:
#####
options(scipen = 999)
dummy=data.frame(UNIT = rep('1A',6),
                 wmu_type = c('Unbuffered','Buffered',
                              'Unbuffered','Buffered',
                              'Unbuffered','Buffered'),
                 index = c(rep("Probability of Connectivity",2),
                           rep("Sinuous Connection Reduction",2),
                           rep("Equivalent Connected Area",2)),
                 value = c(rep(1,6))) %>%
  mutate(index = factor(index,levels=c("Probability of Connectivity",
                                       "Equivalent Connected Area",
                                       "Sinuous Connection Reduction")))
distributions=ggplot(data=metric_data %>%
                       mutate(index = case_when(index == "custom_LS_metric" ~ "Sinuous Connection Reduction",
                                                index == "ECA_double" ~ "Equivalent Connected Area",
                                                index == "PC_index_double" ~ "Probability of Connectivity"),
                              wmu_type = case_when(wmu_type == "buffered" ~ "Buffered",
                                                   wmu_type == "unbuffered" ~ "Unbuffered")) %>%
                       mutate(index = factor(index,levels = c("Probability of Connectivity",
                                                              "Equivalent Connected Area",
                                                              "Sinuous Connection Reduction")),
                              wmu_type = factor(wmu_type,levels=c("Unbuffered","Buffered")),
                              value = ifelse(index=="Equivalent Connected Area",value/1000,value)),
                     aes(x=value))+
  geom_histogram()+
  geom_blank(data=dummy)+
  facet_grid(wmu_type~index,scales='free_x')+
  theme_bw()+
  ylab("Frequency")+
  xlab("Metric value")+
  theme(text=element_text(size=20));distributions

ggsave(distributions,
       filename = paste0(getwd(),'/Figures/Histograms.jpeg'),
       dpi = 300,
       width = 12)
