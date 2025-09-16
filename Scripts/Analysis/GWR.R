#####
# Load regression/analysis datasets:
#####
source(paste0(getwd(),'/Scripts/Analysis/Stat_dataset_compile.R'))

#####
# Run GWR and get average parameters for all years
#####
library(hgwrr)
library(spgwr)

spatial_reg_dat = spatial_reg_dat %>%
  mutate(longitude = st_coordinates(st_centroid(spatial_reg_dat))[,1],
         latitude = st_coordinates(st_centroid(spatial_reg_dat))[,2])
for(i in min(spatial_reg_dat$huntyear):max(spatial_reg_dat$huntyear)){
  custom_gwr_dat = spatial_reg_dat %>%
    filter(index=="custom_LS_metric",
           wmu_type=="unbuffered",
           huntyear==i)
  PC_gwr_dat = spatial_reg_dat %>%
    filter(index=="PC_index_double",
           wmu_type=="unbuffered",
           huntyear==i)
  ECA_gwr_dat = spatial_reg_dat %>%
    filter(index=="ECA_double",
           wmu_type=="unbuffered",
           huntyear==i)
  
  fm = log(Density) ~ value
  custom_band_width=spgwr::gwr.sel(formula = fm,
                                   data = custom_gwr_dat,
                                   coords = cbind(custom_gwr_dat$longitude,
                                                  custom_gwr_dat$latitude),
                                   adapt=F,
                                   verbose = F)
  
  
  custom_gwr_mod=spgwr::gwr(formula = fm,
                            data = custom_gwr_dat,
                            bandwidth=custom_band_width,
                            coords = cbind(custom_gwr_dat$longitude,
                                           custom_gwr_dat$latitude))
  PC_band_width=spgwr::gwr.sel(formula = fm,
                               data = PC_gwr_dat,
                               coords = cbind(PC_gwr_dat$longitude,
                                              PC_gwr_dat$latitude),
                               adapt=F,
                               verbose = F)
  
  
  PC_gwr_mod=spgwr::gwr(formula = fm,
                        data = PC_gwr_dat,
                        bandwidth=PC_band_width,
                        coords = cbind(PC_gwr_dat$longitude,
                                       PC_gwr_dat$latitude))
  ECA_band_width=spgwr::gwr.sel(formula = fm,
                                data = ECA_gwr_dat,
                                coords = cbind(ECA_gwr_dat$longitude,
                                               ECA_gwr_dat$latitude),
                                adapt=F,
                                verbose = F)
  
  
  ECA_gwr_mod=spgwr::gwr(formula = fm,
                         data = ECA_gwr_dat,
                         bandwidth=ECA_band_width,
                         coords = cbind(ECA_gwr_dat$longitude,
                                        ECA_gwr_dat$latitude))
  
  
  
  
  if(i==min(spatial_reg_dat$huntyear)){
    gwr_plot_dat = spatial_reg_dat %>%
      filter(index=="custom_LS_metric",
             wmu_type=="unbuffered",
             huntyear==i) %>%
      mutate(params = custom_gwr_mod$SDF$value) %>%
      bind_rows(.,spatial_reg_dat %>%
                  filter(index=="PC_index_double",
                         wmu_type=="unbuffered",
                         huntyear==i) %>%
                  mutate(params = PC_gwr_mod$SDF$value)) %>%
      bind_rows(.,spatial_reg_dat %>%
                  filter(index=="ECA_double",
                         wmu_type=="unbuffered",
                         huntyear==i) %>%
                  mutate(params = ECA_gwr_mod$SDF$value))
  }
  if(i>min(spatial_reg_dat$huntyear)){
    gwr_plot_dat2 = spatial_reg_dat %>%
      filter(index=="custom_LS_metric",
             wmu_type=="unbuffered",
             huntyear==i) %>%
      mutate(params = custom_gwr_mod$SDF$value) %>%
      bind_rows(.,spatial_reg_dat %>%
                  filter(index=="PC_index_double",
                         wmu_type=="unbuffered",
                         huntyear==i) %>%
                  mutate(params = PC_gwr_mod$SDF$value)) %>%
      bind_rows(.,spatial_reg_dat %>%
                  filter(index=="ECA_double",
                         wmu_type=="unbuffered",
                         huntyear==i) %>%
                  mutate(params = ECA_gwr_mod$SDF$value))
    gwr_plot_dat = rbind(gwr_plot_dat2,gwr_plot_dat)}
}

mean_params = gwr_plot_dat %>%
  group_by(UNIT,index) %>%
  summarize(mn_params = mean(params,na.rm=T)) 

