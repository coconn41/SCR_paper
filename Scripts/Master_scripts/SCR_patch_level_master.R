#####
# Load libraries:
#####
source(paste0(getwd(),"/Scripts/Load_libraries.R"))
#####
# Set calculation parameters:
#####
cores = 4
distance = 1675 # For home-range buffering and SCR threshold distance
#####
# New York State Shapefile Download:
#####
NYS = read_sf(paste(getwd(),"/Data/Input_data/cb_2018_us_state_500k.shp",sep="")) %>%
  filter(NAME=="New York") %>%
  st_transform(.,crs=32618)
#####
# Land Cover Download:
#####
source(paste0(getwd(),"/Scripts/Land_cover_download.R"))

#####
# Roadway Data Cleaning:
#####
source(paste0(getwd(),"/Scripts/Roadway_data_cleaning.R"))

#####
# Alpine Ecozone Data Cleaning:
#####
source(paste0(getwd(),"/Scripts/Alpine_ecozone_cleaning.R"))

#####
# Node processing:
#####
source(paste0(getwd(),"/Scripts/Process_nodes.R"))

#####
# Resistance Raster Processing:
#####
source(paste0(getwd(),"/Scripts/Resistance_raster_processing.R"))

#####
# Least-cost Path Calculations:
#####
source(paste0(getwd(),"/Scripts/Calculate_least_cost_paths.R"))

#####
# Get Euclidean distances:
#####
source(paste0(getwd(),"/Scripts/Get_euclidean_distances.R"))

#####
# Calculate sinuosity:
#####
source(paste0(getwd(),"/Scripts/Calculate_inverse_sinuosity.R"))

#####
# Calculate threshold buffers:
#####
source(paste0(getwd(),"/Scripts/Calculate_buffer_thresholds.R"))

#####
# Merge and save:
#####
source(paste0(getwd(),"/Scripts/Merge_and_save.R"))

