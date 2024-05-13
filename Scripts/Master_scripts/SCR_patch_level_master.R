#####
# Load libraries:
#####
source(paste0(getwd(),"/Scripts/Universal/Load_libraries.R"))
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
template = NYS
source(paste0(getwd(),"/Scripts/Universal/Land_cover_download.R"))

#####
# Roadway Data Cleaning:
#####
source(paste0(getwd(),"/Scripts/Universal/Roadway_data_cleaning.R"))

#####
# Alpine Ecozone Data Cleaning:
#####
source(paste0(getwd(),"/Scripts/SCR/Alpine_ecozone_cleaning.R"))

#####
# Node processing:
#####
source(paste0(getwd(),"/Scripts/Universal/Process_nodes.R"))
nodes = wmu_nodes
remove(wmu_nodes)
#####
# Resistance Raster Processing:
#####
source(paste0(getwd(),"/Scripts/SCR/Resistance_raster_processing.R"))

#####
# Calculate comparison matrix:
#####
source(paste0(getwd(),'/Scripts/SCR/Within_distance_comps.R'))

#####
# Least-cost Path Calculations:
#####
source(paste0(getwd(),"/Scripts/SCR/Calculate_least_cost_paths.R"))

#####
# Get Euclidean distances:
#####
# This includes some reassigning of objects
# that are used in both landscape and patch level metrics
wmu_nodes = nodes
wmus = NYS
source(paste0(getwd(),"/Scripts/SCR/Get_euclidean_distances.R"))
remove(wmu_nodes)
remove(wmus)

#####
# Calculate threshold buffers:
#####
source(paste0(getwd(),"/Scripts/SCR/Calculate_buffer_thresholds.R"))

#####
# Merge and save:
#####
source(paste0(getwd(),"/Scripts/SCR/Merge_and_save.R"))

