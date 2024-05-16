#####
# Load regression/analysis datasets:
#####
source(paste0(getwd(),'/Scripts/Analysis/Stat_dataset_compile.R'))

#####
# Calculate Summary Stats:
#####
metric_data %>%
  group_by(index,wmu_type) %>%
  summarize(mean = mean(value),
            min = min(value),
            max = max(value))
