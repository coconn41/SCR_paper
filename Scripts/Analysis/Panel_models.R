#####
# Load regression/analysis datasets:
#####
source(paste0(getwd(),'/Scripts/Analysis/Stat_dataset_compile.R'))

#####
# A-spatial panel models:
#####
library(plm)
fm = log(Density) ~ value 
SCR_plm = plm::plm(fm,
                     data = reg_dat %>%
                       filter(wmu_type == "unbuffered",
                              index == "custom_LS_metric"),
                     effect='time',
                     model = 'random')
summary(SCR_plm)

PC_plm = plm::plm(fm,
                  data = reg_dat %>%
                    filter(wmu_type == "unbuffered",
                           index == "PC_index_double"),
                  effect='time',
                  model = 'random')
summary(PC_plm)

ECA_plm = plm::plm(fm,
                   data = reg_dat %>%
                     filter(wmu_type == "unbuffered",
                            index == "ECA_double"),
                   effect='time',
                   model = 'random')
summary(ECA_plm)

display_df = data.frame(metric = c("SCR",
                                   "PC",
                                   "ECA"),
                        Beta = c(SCR_plm$coefficients[2],
                                 PC_plm$coefficients[2],
                                 ECA_plm$coefficients[2]),
                        P_value = c(ifelse(summary(SCR_plm)$coefficients[8]<0.0001,"<0.0001",summary(SCR_plm)$coefficients[8]),
                                    ifelse(summary(PC_plm)$coefficients[8]<0.0001,"<0.0001",summary(PC_plm)$coefficients[8]),
                                    ifelse(summary(ECA_plm)$coefficients[8]<0.0001,"<0.0001",summary(ECA_plm)$coefficients[8])),
                        R2 = c(summary(SCR_plm)$r.squared[1],
                               summary(PC_plm)$r.squared[1],
                               summary(ECA_plm)$r.squared[1]))
display_df
