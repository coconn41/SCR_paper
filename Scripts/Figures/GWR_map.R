#####
# Run GWR script:
#####
source(paste0(getwd(),'/Scripts/Analysis/GWR.R'))

#####
# Generate SCR GWR map:
######
custom_params = mean_params %>%
  filter(index=='custom_LS_metric') %>%
  mutate(colors = case_when(mn_params < -4 ~ "< -4",
                            mn_params >= -4 &
                              mn_params < -3 ~ "-4 to -3",
                            mn_params >= -3 &
                              mn_params < -2 ~ "-3 to -2",
                            mn_params >= -2 &
                              mn_params < -1 ~ "-2 to -1",
                            mn_params >= -1 & 
                              mn_params < 0 ~ "-1 to 0",
                            # mn_params == 0 ~ "0",
                            mn_params > 0 & 
                              mn_params < 1 ~ "0 to 1",
                            mn_params >= 1 &
                              mn_params < 2 ~ "1 to 2",
                            mn_params >= 2 &
                              mn_params < 3 ~ "2 to 3",
                            mn_params >= 3 &
                              mn_params ~ "3 to 4",
                            mn_params >= 4 ~ "> 4")) %>%
  mutate(colors = factor(colors,
                         levels=c("< -4",
                                  "-4 to -3",
                                  "-3 to -2",
                                  "-2 to -1",
                                  "-1 to 0",
                                  #  "0",
                                  "0 to 1",
                                  "1 to 2",
                                  "2 to 3",
                                  "3 to 4",
                                  "> 4")))

custom_parameter_map=tm_shape(custom_params)+
  tm_polygons(col='colors',
              palette=tmaptools::get_brewer_pal("PRGn",n=10),
              title="Sinuous Connection Reduction")+
  tm_layout(main.title="C",
            legend.title.size = 1.5,
            legend.text.size = .9)+
  tm_scale_bar(position=c('left','bottom'))

#####
# Generate PC GWR map:
#####
PC_params = mean_params %>%
  filter(index=="PC_index_double") %>%
  mutate(colors = case_when(mn_params < -4 ~ "< -4",
                            mn_params >= -4 &
                              mn_params < -3 ~ "-4 to -3",
                            mn_params >= -3 &
                              mn_params < -2 ~ "-3 to -2",
                            mn_params >= -2 &
                              mn_params < -1 ~ "-2 to -1",
                            mn_params >= -1 &
                              mn_params < 0 ~ "-1 to 0",
                            mn_params >= 0 & 
                              mn_params < 1 ~ "0 to 1",
                            mn_params >= 1 &
                              mn_params < 2 ~ "1 to 2",
                            mn_params >= 2 &
                              mn_params < 3 ~ "2 to 3",
                            mn_params >= 3 &
                              mn_params < 4 ~ "3 to 4",
                            mn_params >= 4 ~ "> 4")) %>%
  mutate(colors = factor(colors,
                         levels=c("< -4",
                                  "-4 to -3",
                                  "-3 to -2",
                                  "-2 to -1",
                                  "-1 to 0",
                                  "0 to 1",
                                  "1 to 2",
                                  "2 to 3",
                                  "3 to 4",
                                  "> 4")))

PC_parameter_map=tm_shape(PC_params)+
  tm_polygons(col='colors',
              palette = tmaptools::get_brewer_pal("PRGn",n=10),
              title="Probability of Connectivity")+
  tm_layout(legend.title.size=1.5,
            main.title = "A",
            legend.text.size = .9)+
  tm_scale_bar(position=c('left','bottom'))

#####
# Generate ECA GWR map:
#####
ECA_params = mean_params %>%
  filter(index=="ECA_double") %>%
  mutate(colors = case_when(mn_params < -.000015 ~ "< -1.5e-5",
                            mn_params >= -.000015 &
                              mn_params < -.0000075 ~ "-1.5e-5 to -7.5e-6",
                            mn_params >= -.0000075 &
                              mn_params < 0 ~ "-7.5e-6 to 0",
                            mn_params >= 0 &
                              mn_params < .0000075 ~ "0 to 7.5e-6",
                            mn_params >= .0000075 &
                              mn_params < .000015 ~ "7.5e-6 to 1.5e-5",
                            mn_params >= .000015 &
                              mn_params < .0000225 ~ "1.5e-5 to 2.25e-5",
                            mn_params >= .0000225 &
                              mn_params < .00003 ~ "2.25e-5 to 3.0e-5",
                            mn_params >= .00003 ~ "> 3.0e-5")) %>%
  mutate(colors = factor(colors,
                         levels=c("< -1.5e-5",
                                  "-1.5e-5 to -7.5e-6",
                                  "-7.5e-6 to 0",
                                  "0 to 7.5e-6",
                                  "7.5e-6 to 1.5e-5",
                                  "1.5e-5 to 2.25e-5",
                                  "2.25e-5 to 3.0e-5",
                                  "> 3.0e-5")))

ECA_parameter_map=tm_shape(ECA_params)+
  tm_polygons(col='colors',
              palette=tmaptools::get_brewer_pal("PRGn"),
              title="Equivalent Connected Area")+
  tm_layout(main.title="B",
            legend.title.size=1.5,
            legend.text.size = .9)+
  tm_scale_bar(position=c('left','bottom'))

#####
# Generate average deer take density map:
#####
avg_deer_take = spatial_reg_dat %>%
  group_by(UNIT) %>%
  summarize(mn_take_density = mean(Total/area,na.rm=T))

Deer_take_map=tm_shape(avg_deer_take)+
  tm_polygons(col='mn_take_density',
              palette=tmaptools::get_brewer_pal("Greens",n=5),
              title="Average yearly deer\ntake per hectare")+
  tm_layout(main.title="D",
            legend.title.size = 1.5,
            legend.text.size = .9)+
  tm_scale_bar(position=c('left','bottom'))

#####
# Compile all maps 
#####
Parameter_map = tmap_arrange(PC_parameter_map,ECA_parameter_map,
                             custom_parameter_map,Deer_take_map,ncol=2)

#####
# Save map:
#####
tmap_save(Parameter_map,
          filename=paste0(getwd(),'/Figures/Parameter_maps_scalebar.jpeg'),
          dpi = 300,
          height=12)
