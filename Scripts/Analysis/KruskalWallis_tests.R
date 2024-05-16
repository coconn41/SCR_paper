#####
# Load regression/analysis datasets:
#####
source(paste0(getwd(),'/Scripts/Analysis/Stat_dataset_compile.R'))

#####
# Test for edge effects: (Kruskal-Wallis)
#####
r1=kruskal.test(value~wmu_type,
             data=metric_data %>% filter(index=="ECA_double"))
r2=kruskal.test(value~wmu_type,
             data=metric_data %>% filter(index=="PC_index_double"))
r3=kruskal.test(value~wmu_type,
             data=metric_data %>% filter(index=="custom_LS_metric"))

display_df = data.frame(Metric = c("ECA",
                                   "PC",
                                   "SCR"),
                        P_value = c(r1$p.value,
                                    r2$p.value,
                                    r3$p.value))
display_df
