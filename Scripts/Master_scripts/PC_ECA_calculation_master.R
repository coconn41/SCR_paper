#set wd
setwd("/user/collinoc/SCR_paper/")
#####
# Load libraries:
#####
source(paste0(getwd(),"/Scripts/Universal/Load_libraries.R"))
#####
# Set calculation parameters:
#####
tdir = tempdir()
cores = 48
distance = 1675 # For home-range buffering and SCR threshold distance
theta = .001788497 # Theta in PC / ECA index calculation
buff_unbuff = "Unbuffered"
#####
# WMU Shapefile and Land Cover Download:
#####
wmus = read_sf(paste0(getwd(),"/Data/Input_data/WMUs/Wildlife_Management_Units.shp"))
template = read_sf(paste0(getwd(),'/Data/Input_data/cb_2018_us_state_500k.shp')) %>%
  filter(NAME == "New York")
use_cached_LC = TRUE
if(use_cached_LC==FALSE){source(paste0(getwd(),"/Scripts/Universal/Land_cover_download.R"))}
if(use_cached_LC==TRUE){LC = raster::raster(paste0(getwd(),'/Data/Input_data/NYS NLCD_NLCD_Land_Cover_2019.tif'))
LCr = rast(LC)
LCproj = terra::project(LCr,crs(template))

LCcrop = terra::crop(x = LCproj,
                     y = template |>
                       terra::vect(),
                     mask = T)}

#####
# Loop through analysis
#####
if(buff_unbuff=="Buffered"){
remainingWMUS = subset(unique(wmus$UNIT),
                       !unique(wmus$UNIT)%in%substring(list.files(paste0(getwd(),'/Data/Output_data/PC_ECA/Buffered/')),
                          10,11))}
if(buff_unbuff!="Buffered"){
  remainingWMUS = subset(unique(wmus$UNIT),
                         !unique(wmus$UNIT)%in%substring(list.files(paste0(getwd(),'/Data/Output_data/PC_ECA/Unbuffered/')),
                                                         12,13))}
xind=0
for(x in remainingWMUS){
  print(paste0(xind,"_",x))
  xind=xind+1
  sing_wmu = wmus %>% filter(UNIT==x) 
  if(buff_unbuff=="Buffered"){sing_wmu = sing_wmu %>%
    st_buffer(.,dist=distance)}
  sing_wmu$area=st_area(sing_wmu)
  sing_wmu$area = set_units(sing_wmu$area,ha)
  attributes(sing_wmu$area)=NULL

#####
# Crop Land Cover
#####
LCcrop = terra::crop(x = LCproj,
                     y = sing_wmu |>
                     terra::vect(),
                     mask = T)
  
#####
# Node processing:
#####
source(paste0(getwd(),"/Scripts/Universal/Process_nodes.R"))

#####
# Calculate vertex comparison pairs:
#####
comb_mat = matrix(nrow=nrow(wmu_nodes),
                  ncol = nrow(wmu_nodes))  
combinations <- which(upper.tri(comb_mat,
                                diag = FALSE),
                      arr.ind = TRUE)
combinations=as.data.frame(cbind(combinations,
                                 comb_mat[combinations]))
names(combinations)=c('i','j','Nas')
combinations = combinations %>%
  arrange(i) %>%
  dplyr::select(-Nas)

#####
# Calculate vertex distances and areas
#####
source(paste0(getwd(),'/Scripts/PC_ECA/Distance_calculation.R'))

#####
# Calculate graph and final metric values
#####
source(paste0(getwd(),'/Scripts/PC_ECA/Graph_calculation.R'))

#####
# Save files:
#####
source(paste0(getwd(),'/Scripts/PC_ECA/Save_PC_ECA.R'))
}
