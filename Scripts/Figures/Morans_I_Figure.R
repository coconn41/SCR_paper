#####
# Run Moran's I:
#####
source(paste0(getwd(),'/Scripts/Analysis/Morans_I.R'))

#####
# Make supplementary Moran's I plot:
#####
morandf$rej = ifelse(morandf$adjustedps<0.05,"sig.","ns")
morandf$Significance = factor(morandf$rej,levels=c('sig.','ns'))
Moran_plot=ggplot(data=morandf %>%
                    mutate(metric = ifelse(metric=="Custom","Sinuous Connection Reduction",
                                           ifelse(metric=="ECA","Equivalent Connected Area",
                                                  "Probability of Connectivity"))) %>%
                    mutate(metric = factor(metric,levels=c("Probability of Connectivity",
                                                           "Equivalent Connected Area",
                                                           "Sinuous Connection Reduction"))),
                  aes(x=Year,y=moranI,alpha = Significance))+
  geom_point()+
  scale_alpha_manual(values=c(.9,.25))+
  #geom_hline(yintercept=0.05)+
  facet_wrap(.~metric)+
  ylab("Moran's Index")+
  theme_bw()+
  theme(legend.title=element_blank())
#####
# Save file
#####
ggsave(Moran_plot,
       filename = paste0(getwd(),'/Figures/Moran_plot.tiff'),
       dpi = 300,
       width = 12,
       height = 4)
