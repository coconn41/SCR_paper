#####
# Build panel models:
#####
source(paste0(getwd(),'/Scripts/Analysis/Panel_models.R'))

#####
# DW test for serial autocorrelation
#####
r1=plm::pdwtest(SCR_plm)
r2=plm::pdwtest(PC_plm)
r3=plm::pdwtest(ECA_plm)

display_df = data.frame(metric = c("SCR",
                                   "PC",
                                   "ECA"),
                        DW = c(r1$statistic,
                               r2$statistic,
                               r3$statistic),
                        P_value = c(r1$p.value,
                                    r2$p.value,
                                    r3$p.value))
display_df
