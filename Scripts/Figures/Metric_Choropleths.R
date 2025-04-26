#####
# Load regression/analysis datasets:
#####
source(paste0(getwd(),'/Scripts/Analysis/Stat_dataset_compile.R'))

#####
# Set temporary directory:
#####
tdir = tempdir()

#####
# Make map SCR map:
#####
SCRmap = tm_shape(left_join(metric_data %>%
                              filter(wmu_type=="unbuffered",
                                     index=="custom_LS_metric"),wmus,by="UNIT") %>%
                    st_set_geometry(.,'geometry'))+
  tm_polygons(col='value',
              #style='cont',
              breaks=c(0,.25,.5,.75,1),
              title="Sinuous Connection Reduction",
              palette=tmaptools::get_brewer_pal("Greens"))+
  tm_layout(main.title='C',
            legend.title.size = 1.5)+
  tm_scale_bar(position=c('left','bottom'))

#####
# Make PC map:
#####
PCmap = tm_shape(left_join(metric_data %>%
                             filter(wmu_type=="unbuffered",
                                    index=="PC_index_double"),wmus,by="UNIT") %>%
                   st_set_geometry(.,'geometry'))+
  tm_polygons(col='value',
              #style='cont',
              breaks=c(0,.25,.5,.75,1),
              title="Probability of Connectivity",
              palette=tmaptools::get_brewer_pal("Greens"))+
  tm_layout(main.title="A",
            legend.title.size = 1.5)+
  tm_scale_bar(position=c('left','bottom'))

#####
# Make ECA map:
#####
ECAmap = tm_shape(left_join(metric_data %>%
                              filter(wmu_type=="unbuffered",
                                     index=="ECA_double"),wmus,by="UNIT") %>%
                    st_set_geometry(.,'geometry'))+
  tm_polygons(col='value',
              #style='cont',
              title="Equivalent Connected Area",
              palette=tmaptools::get_brewer_pal("Greens"))+
  tm_layout(main.title="B",
            legend.title.size = 1.5)+
  tm_scale_bar(position=c('left','bottom'))

#####
# Make Forested Area map:
#####
LC = get_nlcd(template=st_union(wmus) %>%
                st_simplify(.,dTolerance=500),
              label="NYS NLCD",
              dataset='landcover',
              year=2019,
              landmass = 'L48',
              force.redo = T,
              extraction.dir = tdir)
LCr = rast(LC)
LCproj = terra::project(LCr,crs(wmus))

LCcrop = terra::crop(x = LCproj,
                     y = wmus |>
                       terra::vect(),
                     mask = T)

LC_forest_patches = LCcrop
values(LC_forest_patches)[values(LC_forest_patches)==42] = 41
values(LC_forest_patches)[values(LC_forest_patches)==43] = 41
values(LC_forest_patches)[values(LC_forest_patches)!=41] = NA

LCmap = tm_shape(wmus %>% st_union())+
  tm_borders()+
  tm_shape(LC_forest_patches)+
  tm_raster(legend.show=F,
            col = "NLCD.Land.Cover.Class",
            palette="#00441B")+
  tm_add_legend(type='fill',
                col="#00441B",
                labels = "Forested Area")+
  tm_layout(main.title="D",
            legend.position = c('left','top'))+
  tm_scale_bar(position=c('left','bottom'))

#####
# Arrange maps:
#####
tmar = tmap_arrange(PCmap,ECAmap,SCRmap,LCmap,
                    ncol=2)

#####
# Save maps:
#####

tmap_save(tmar,
          filename=paste0(getwd(),'/Figures/Metric_maps.jpeg'),
          dpi = 300)
