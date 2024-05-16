#####
# Load regression/analysis datasets:
#####
source(paste0(getwd(),'/Scripts/Analysis/Stat_dataset_compile.R'))

#####
# Build spatial panel models:
#####
library(splm)
library(spdep)
custom_dat = reg_dat %>%
  filter(wmu_type=="unbuffered",
         index=="custom_LS_metric")
wmus_rm_custom = wmus %>% filter(c(UNIT %in% unique(custom_dat$UNIT)))

custom_wmu_mat = nb2mat(poly2nb(wmus_rm_custom,
                                row.names = row.names(wmus_rm_custom$UNIT)),
                        zero.policy=T)
colnames(custom_wmu_mat)=row.names(custom_wmu_mat)

PC_index_dat = reg_dat %>%
  filter(wmu_type=="unbuffered",
         index=="PC_index_double") %>%
  mutate(value = value)
wmus_rm_PC = wmus %>% filter(c(UNIT %in% unique(PC_index_dat$UNIT)))

PC_wmu_mat = nb2mat(poly2nb(wmus_rm_PC,
                            row.names = row.names(wmus_rm_PC$UNIT)),
                    zero.policy=T)
colnames(PC_wmu_mat)=row.names(PC_wmu_mat)

ECA_index_dat = reg_dat %>%
  filter(wmu_type=="unbuffered",
         index=="ECA_double")
wmus_rm_ECA = wmus %>% filter(c(UNIT %in% unique(ECA_index_dat$UNIT)))

ECA_wmu_mat = nb2mat(poly2nb(wmus_rm_ECA,
                             row.names = row.names(wmus_rm_ECA$UNIT)),
                     zero.policy=T)
colnames(ECA_wmu_mat)=row.names(ECA_wmu_mat)
nullfm = log(Density) ~ 1
fm = log(Density) ~ value 

panel_model_null = spreml(formula = nullfm,
                          data = custom_dat,
                          w = mat2listw(custom_wmu_mat),
                          model = 'random',
                          lag = TRUE,
                          errors = 'sr')
summary(panel_model_null)

panel_model_custom = spreml(formula = fm,
                            data = custom_dat,
                            w = mat2listw(custom_wmu_mat),
                            model = 'random',
                            lag = TRUE,
                            errors = 'sr')
summary(panel_model_custom)

panel_model_PC = spreml(formula = fm,
                        data = PC_index_dat,
                        w = mat2listw(PC_wmu_mat),
                        model = 'random',
                        lag = TRUE,
                        errors = 'sr')
summary(panel_model_PC)
# This is value * 1000

panel_model_ECA = spreml(formula = fm,
                         data = ECA_index_dat,
                         w = mat2listw(ECA_wmu_mat),
                         model = 'random',
                         lag = TRUE,
                         errors = 'sr')
summary(panel_model_ECA)

panel_model_null$logLik
panel_model_custom$logLik
panel_model_PC$logLik
panel_model_ECA$logLik

display_df = data.frame(metric = c("SCR",
                                   "PC",
                                   "ECA",
                                   "Null"),
                        Beta = c(panel_model_custom$coefficients[2],
                                 panel_model_PC$coefficients[2],
                                 panel_model_ECA$coefficients[2],
                                 NA),
                        P_value = c(ifelse(summary(panel_model_custom)$CoefTable[8]<0.0001,"<0.0001",summary(panel_model_custom)$CoefTable[8]),
                        ifelse(summary(panel_model_PC)$CoefTable[8]<0.0001,"<0.0001",summary(panel_model_PC)$CoefTable[8]),
                        ifelse(summary(panel_model_ECA)$CoefTable[8]<0.0001,"<0.0001",summary(panel_model_ECA)$CoefTable[8]),
                        NA),
                        loglik = c(panel_model_custom$logLik,
                                   panel_model_PC$logLik,
                                   panel_model_ECA$logLik,
                                   panel_model_null$logLik))
display_df                        

loglik_compare_matrix = matrix(data = c(display_df$loglik[1]-display_df$loglik[1],
                                        0,
                                        0,
                                        0,
                                        display_df$loglik[2]-display_df$loglik[1],
                                        display_df$loglik[2]-display_df$loglik[2],
                                        0,
                                        0,
                                        display_df$loglik[3]-display_df$loglik[1],
                                        display_df$loglik[3]-display_df$loglik[2],
                                        display_df$loglik[3]-display_df$loglik[3],
                                        0,
                                        display_df$loglik[4]-display_df$loglik[1],
                                        display_df$loglik[4]-display_df$loglik[2],
                                        display_df$loglik[4]-display_df$loglik[3],
                                        display_df$loglik[4]-display_df$loglik[4]),
                               ncol = 4,
                               dimnames = list(display_df$metric))
colnames(loglik_compare_matrix)=display_df$metric

loglik_compare_matrix
# row subtracted by column 
