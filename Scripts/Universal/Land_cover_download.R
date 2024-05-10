LC = get_nlcd(template=NYS,
              label="NYS NLCD",
              dataset='landcover',
              year=2019,
              landmass = 'L48',
              force.redo = T,
              extraction.dir = tdir)
LCr = rast(LC)
LCproj = terra::project(LCr,crs(NYS))

LCcrop = terra::crop(x = LCproj,
                     y = NYS |>
                       terra::vect(),
                     mask = T)