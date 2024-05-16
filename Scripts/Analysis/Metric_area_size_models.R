#####
# Load regression/analysis datasets:
#####
source(paste0(getwd(),'/Scripts/Analysis/Stat_dataset_compile.R'))

#####
# GAMs and LMs assessing relationship to size:
#####
library(mgcv)
custom_size_lm = lm(formula = log(value) ~ log(area),
                    data = spatial_reg_dat %>%
                      filter(index=="custom_LS_metric",
                             wmu_type=="unbuffered"))
summary(custom_size_lm)

custom_size_gam = gam(formula = value ~ s(area),
                      data = spatial_reg_dat %>%
                        filter(index=="custom_LS_metric",
                               wmu_type=="unbuffered"))
summary(custom_size_gam)

PC_size_lm = lm(formula = value ~ area,
                data = spatial_reg_dat %>%
                  filter(index=="PC_index_double",
                         wmu_type=="unbuffered"))
summary(PC_size_lm)

PC_size_gam = gam(formula = value ~ s(area),
                  data = spatial_reg_dat %>%
                    filter(index=="PC_index_double",
                           wmu_type == "unbuffered"))
summary(PC_size_gam)

ECA_size_lm = lm(formula = value ~ area,
                 data = spatial_reg_dat %>%
                   filter(index=="ECA_double",
                          wmu_type=="unbuffered"))
summary(ECA_size_lm)

ECA_size_gam = gam(formula = value ~ s(area),
                   data = spatial_reg_dat %>%
                     filter(index=="ECA_double",
                            wmu_type == "unbuffered"))
summary(ECA_size_gam)

display_df = data.frame(metric = rep(c("SCR",
                                   "PC",
                                   "ECA"),2),
                        method = c(rep("GAM",3),rep("LM",3)),
                        R2 = c(summary(custom_size_gam)$r.sq,
                               summary(PC_size_lm)$r.sq,
                               summary(ECA_size_gam)$r.sq,
                               summary(custom_size_lm)$r.squared,
                               summary(PC_size_lm)$r.squared,
                               summary(ECA_size_lm)$r.squared))
display_df

C_AIC = AIC(custom_size_gam);C_AIC
PC_AIC = AIC(PC_size_gam);PC_AIC
ECA_AIC = AIC(ECA_size_gam);ECA_AIC

m = matrix(c((C_AIC-C_AIC),(C_AIC-PC_AIC),(C_AIC-ECA_AIC),
             (PC_AIC-C_AIC),(PC_AIC-PC_AIC),(PC_AIC-ECA_AIC),
             (ECA_AIC-C_AIC),(ECA_AIC-PC_AIC),(ECA_AIC-ECA_AIC)),
           ncol=3,dimnames = list(c("SRC","PC","ECA")))
colnames(m)=c('SRC','PC','ECA')
m
