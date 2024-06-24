#####
# Load regression/analysis datasets:
#####
source(paste0(getwd(),'/Scripts/Analysis/Stat_dataset_compile.R'))

#####
# Plot histograms:
#####
options(scipen = 999)
dummy=data.frame(UNIT = rep('1A',6),
                 `WMU type` = c('Unbuffered','Buffered',
                              'Unbuffered','Buffered',
                              'Unbuffered','Buffered'),
                 index = c(rep("Probability of Connectivity (PC)",2),
                           rep("Sinuous Connection Reduction (SCR)",2),
                           rep("Equivalent Connected Area (ECA)",2)),
                 value = c(rep(1,6))) %>%
  mutate(index = factor(index,levels=c("Probability of Connectivity (PC)",
                                       "Equivalent Connected Area (ECA)",
                                       "Sinuous Connection Reduction (SCR)")))
distributions=ggplot(data=metric_data %>%
                       mutate(index = case_when(index == "custom_LS_metric" ~ "Sinuous Connection Reduction (SCR)",
                                                index == "ECA_double" ~ "Equivalent Connected Area (ECA)",
                                                index == "PC_index_double" ~ "Probability of Connectivity (PC)"),
                              `WMU type` = case_when(wmu_type == "buffered" ~ "Buffered",
                                                   wmu_type == "unbuffered" ~ "Unbuffered")) %>%
                       mutate(index = factor(index,levels = c("Probability of Connectivity (PC)",
                                                              "Equivalent Connected Area (ECA)",
                                                              "Sinuous Connection Reduction (SCR)")),
                              `WMU type` = factor(wmu_type,levels=c("Unbuffered","Buffered")),
                              value = ifelse(index=="Equivalent Connected Area (ECA)",value/1000,value)),
                     aes(x=value))+
  geom_histogram()+
  geom_blank(data=dummy)+
  facet_grid(`WMU type`~index,scales='free_x')+
  theme_bw()+
  ylab("Frequency")+
  xlab("Metric value")+
  theme(text=element_text(size=20));distributions

ggsave(distributions,
       filename = paste0(getwd(),'/Figures/Histograms.jpeg'),
       dpi = 300,
       width = 12)

options(scipen = 999)
densities = ggplot(data=metric_data %>%
  mutate(index = case_when(index == "custom_LS_metric" ~ "Sinuous Connection Reduction (SCR)",
                           index == "ECA_double" ~ "Equivalent Connected Area (ECA)",
                           index == "PC_index_double" ~ "Probability of Connectivity (PC)"),
         `WMU type` = case_when(wmu_type == "buffered" ~ "Buffered",
                                wmu_type == "unbuffered" ~ "Unbuffered")) %>%
  mutate(index = factor(index,levels = c("Probability of Connectivity (PC)",
                                         "Equivalent Connected Area (ECA)",
                                         "Sinuous Connection Reduction (SCR)")),
         `WMU type` = factor(`WMU type`,levels=c("Unbuffered","Buffered")),
         value = ifelse(index=="Equivalent Connected Area (ECA)",value/1000,value)),
aes(x=value,group=`WMU type`,fill=`WMU type`))+
  geom_density(alpha=.6)+
  geom_blank(data=dummy %>%
               mutate(`WMU type`=`WMU.type`))+
  facet_wrap(.~index,scales='free')+
  theme_bw()+
  scale_fill_manual(values=c("#E7298A","#1B9E77"))+
  ylab("Density")+
  xlab("Metric value");densities

ggsave(densities,
       filename = paste0(getwd(),'/Figures/Densities.jpeg'),
       dpi = 300,
       width = 12)
