LC = get_nlcd(template=template,
              label="NLCD",
              dataset='landcover',
              year=2019,
              landmass = 'L48',
              force.redo = T,
              extraction.dir = paste0(getwd(),'/Data/Input_data/'))
LCr = rast(LC)
LCproj = terra::project(LC,template)

LCcrop = terra::crop(x = LCproj,
                     y = template |>
                       terra::vect(),
                     mask = T)
