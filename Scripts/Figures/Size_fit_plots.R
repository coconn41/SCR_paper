#####
# Load regression/analysis datasets:
#####
source(paste0(getwd(),'/Scripts/Analysis/Stat_dataset_compile.R'))

#####
# Generate Supplemental figure 2
#####
fitplot=ggplot(data=spatial_reg_dat %>%
                 filter(wmu_type=="unbuffered") %>%
                 mutate(index = ifelse(index=="custom_LS_metric","Sinuous Connection Reduction",
                                       ifelse(index=="ECA_double","Equivalent Connected Area",
                                              "Probability of Connectivity"))) %>%
                 mutate(index = factor(index,levels=c("Probability of Connectivity",
                                                      "Equivalent Connected Area",
                                                      "Sinuous Connection Reduction"))),
               aes(x=area,y=value))+
  geom_point()+
  geom_smooth(col='blue',method = 'lm',se=F)+
  geom_smooth(col='red',method = 'gam',se=F,
              formula = y ~ s(x))+
  #geom_smooth(col='orange',method='loess',se=F)+
  facet_wrap(.~index,scales="free_y")+
  ylab("Metric value")+
  xlab("Area (hectares)")+
  theme_bw()+
  theme(text=element_text(size=20))
#####
# Save figure
#####

ggsave(fitplot,
      filename = paste0(getwd(),"/Figures/Size_fit.jpeg"),
      dpi = 300,
      width=14)