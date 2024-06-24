#####
# Load libraries:
#####
source(paste0(getwd(),'/Scripts/Universal/Load_libraries.R'))
#####
# Load wmus and deer data:
#####
wmus = read_sf(paste0(getwd(),"/Data/Input_data/WMUS/Wildlife_Management_Units.shp")) %>%
  st_transform(.,32618)
deer_data <- read_excel(paste0(getwd(),"/Data/Input_data/Deer_data.xlsx"))
names(deer_data)[1]="UNIT"
#####
# Compile results data:
#####
unbuf_files = list.files(paste0(getwd(),"/Data/Output_data/PC_ECA/Unbuffered"))
buf_files = list.files(paste0(getwd(),'/Data/Output_data/PC_ECA/Buffered/'))
ind=0
for(i in 1:length(buf_files)){
  ind=ind+1
  fl = read.csv(paste0(getwd(),"/Data/Output_data/PC_ECA/Buffered/",buf_files[i]))
  fl$wmu_type="buffered"
  if(ind==1){fl2 = fl}
  if(ind>1){fl2 = rbind(fl,fl2)}
}
for(i in 1:length(unbuf_files)){
  ind=ind+1
  fl = read.csv(paste0(getwd(),"/Data/Output_data/PC_ECA/Unbuffered/",unbuf_files[i]))
  fl$wmu_type="unbuffered"
  fl2 = rbind(fl,fl2) 
}
unbuf_custom_files = list.files(paste0(getwd(),"/Data/Output_data/SCR/Landscape/Unbuffered/"))
buf_custom_files = list.files(paste0(getwd(),"/Data/Output_data/SCR/Landscape/Buffered/"))
ind=0
for(i in 1:length(buf_custom_files)){
  ind=ind+1
  cust = read.csv(paste0(getwd(),"/Data/Output_data/SCR/Landscape/Buffered/",buf_custom_files[i]))
  cust$wmu_type = "buffered"
  names(cust)[3]="index"
  if(ind==1){cust2=cust}
  if(ind>1){cust2 = rbind(cust,cust2)}
}
for(i in 1:length(unbuf_custom_files)){
  cust = read.csv(paste0(getwd(),"/Data/Output_data/SCR/Landscape/Unbuffered/",unbuf_custom_files[i]))
  cust$wmu_type = "unbuffered"
  names(cust)[3]="index"
  cust2 = rbind(cust2,cust)
}
metric_data = fl2 %>%
  bind_rows(.,cust2) %>%
  filter(index %in% c("PC_index_double",
                      "ECA_double",
                      "custom_LS_metric")) %>%
  select(-X)
#####
# Build regression datasets (reg_dat and spatial_reg_dat):
#####

reg_dat = left_join(deer_data,
                    metric_data,
                    by="UNIT",
                    relationship="many-to-many") %>%
  left_join(.,wmus %>%
              mutate(area = st_area(.)) %>%
              st_drop_geometry()) %>%
  mutate(Region = factor(substring(.$UNIT,1,1),
                         levels=c(1:9)))
reg_dat$area = set_units(reg_dat$area,ha)
attributes(reg_dat$area)=NULL
reg_dat$Density = reg_dat$Total/reg_dat$area

spatial_reg_dat = left_join(reg_dat,wmus,by="UNIT") %>%
  st_set_geometry(.,'geometry')
