#####
# Build panel models:
#####
source(paste0(getwd(),'/Scripts/Analysis/Panel_models.R'))

#####
# Run Moran's I for each year of a-spatial panel models:
#####
library(spdep)
SCR_residdf = spatial_reg_dat %>%
  filter(wmu_type == "unbuffered",
         index == "custom_LS_metric") %>%
  mutate(SCR_plm_resid = SCR_plm$residuals)
PC_residdf = spatial_reg_dat %>%
  filter(wmu_type == "unbuffered",
         index == "PC_index_double") %>%
  mutate(PC_plm_resid = PC_plm$residuals)
ECA_residdf = spatial_reg_dat %>%
  filter(wmu_type == "unbuffered",
         index == "PC_index_double") %>%
  mutate(ECA_plm_resid = ECA_plm$residuals)
ind = 0
for(i in min(SCR_residdf$huntyear):max(SCR_residdf$huntyear)){
  ind=ind+1
  df = SCR_residdf %>%
    filter(huntyear==i)
  reduce_wmus = wmus %>%
    filter(c(UNIT %in% unique(df$UNIT)))
  
  nbs = poly2nb(reduce_wmus,
                row.names = row.names(reduce_wmus$UNIT))
  
  SCR_moran = moran.test(df$SCR_plm_resid,
                            nb2listw(nbs,zero.policy = T),
                            zero.policy = T)
  
  PCdf = PC_residdf %>%
    filter(huntyear == i)
  reduce_wmus = wmus %>%
    filter(c(UNIT %in% unique(PCdf$UNIT)))
  PCnbs = poly2nb(reduce_wmus,
                  row.names = row.names(reduce_wmus$UNIT))
  PC_moran = moran.test(PCdf$PC_plm_resid,
                        nb2listw(PCnbs,zero.policy = T),
                        zero.policy = T)
  
  ECAdf = ECA_residdf %>%
    filter(huntyear == i) 
  reduce_wmus = wmus %>%
    filter(c(UNIT %in% unique(PCdf$UNIT)))
  ECAnbs = poly2nb(reduce_wmus,
                   row.names = row.names(reduce_wmus$UNIT))
  ECA_moran = moran.test(ECAdf$ECA_plm_resid,
                         nb2listw(ECAnbs,zero.policy=T),
                         zero.policy = T)
  if(ind == 1){morandf = data.frame(metric = c("Custom",
                                               "PC",
                                               "ECA"),
                                    Year = c(i,i,i),
                                    pval = c(SCR_moran$p.value,
                                             PC_moran$p.value,
                                             ECA_moran$p.value),
                                    moranI = c(SCR_moran$estimate[1],
                                               PC_moran$estimate[1],
                                               ECA_moran$estimate[1]))}
  if(ind>1){morandf2 = data.frame(metric = c("Custom",
                                             "PC",
                                             "ECA"),
                                  Year = c(i,i,i),
                                  pval = c(SCR_moran$p.value,
                                           PC_moran$p.value,
                                           ECA_moran$p.value),
                                  moranI = c(SCR_moran$estimate[1],
                                             PC_moran$estimate[1],
                                             ECA_moran$estimate[1]))
  morandf = rbind(morandf,morandf2)}
}

#####
# Adjust p-values
#####
morandf$adjustedps = p.adjust(morandf$pval,method = "BH")