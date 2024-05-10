#####
# Load libraries:
#####
source(paste0(getwd(),"/Scripts/Universal/Load_libraries.R"))
#####
# Set calculation parameters:
#####
cores = 4
distance = 1675 # For home-range buffering and SCR threshold distance
theta = .001788497 # Theta in PC / ECA index calculation
buff_unbuff = "Buffered"
#####
# WMU Shapefile and Land Cover Download:
#####
wmus = read_sf(paste0(getwd(),"/Data/Input_data/Wildlife_Management_Units_5181502506492967745/Wildlife_Management_Units.shp"))

source(paste0(getwd(),"/Scripts/Universal/Land_cover_download.R"))

NYS_LC = get_nlcd(template = NYS %>%
                    st_transform(.,crs=st_crs(wmus)),
         year = 2019,
         landmass = 'L48',
         label = "Forests")

LCr = rast(NYS_LC)
wmus = st_transform(wmus,crs(LCr))
LCproj = terra::project(LCr,crs(wmus))

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
}