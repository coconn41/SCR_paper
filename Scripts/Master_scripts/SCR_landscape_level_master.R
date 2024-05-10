#####
# Load libraries:
#####
source(paste0(getwd(),"/Scripts/Universal/Load_libraries.R"))
#####
# Set calculation parameters:
#####
cores = 4
distance = 1675 # For home-range buffering and SCR threshold distance
buff_unbuff = "Buffered"

#####
# WMU Shapefile download:
#####

wmus = read_sf(paste0(getwd(),"/Data/Input_data/Wildlife_Management_Units_5181502506492967745/Wildlife_Management_Units.shp"))

#####
# Loop through analysis
#####

xind=0
for(x in unique(wmus$UNIT)){
  print(paste0(xind,"_",x))
  xind=xind+1
  sing_wmu = wmus %>% filter(UNIT==x) 
  if(buff_unbuff=="Buffered"){sing_wmu = sing_wmu %>%
    st_buffer(.,dist=distance)}
  sing_wmu$area=st_area(sing_wmu)
  sing_wmu$area = set_units(sing_wmu$area,ha)
  attributes(sing_wmu$area)=NULL

#####
# Land Cover Download
#####
template = sing_wmu
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
  
#####
# Resistance Raster Processing:
#####
source(paste0(getwd(),"/Scripts/SCR/Resistance_raster_processing.R"))

#####
# Landscape level metric comparisons:
#####
source(paste0(getwd(),"/Scripts/SCR/Within_distance_comps.R"))

#####
# Landscape level metric comparisons:
#####
source(paste0(getwd(),"/Scrips/SCR/Landscape_network.R"))
  
#####
# Get Euclidean Distances
#####
source(paste0(getwd(),"/Scripts/SCR/Get_euclidean_distances.R"))
  
#####
# Combine and save
#####
source(paste0(getwd(),"/Scripts/SCR/Landscape_Merge_and_save.R"))
  
}