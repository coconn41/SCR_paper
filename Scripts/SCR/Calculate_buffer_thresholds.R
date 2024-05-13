myCluster <- parallel::makeCluster(cores)
doParallel::registerDoParallel(myCluster)

buffer_polys <- foreach::foreach(i = 1:nrow(fin_poly), .errorhandling = "remove", .combine = "rbind",.packages = c("sf", "dplyr")) %dopar% {
  
bufs = fin_poly[i,] %>%
  mutate(i_area = st_area(.)) %>%
  st_buffer(.,dist=distance) %>%
  mutate(di_area = st_area(.))

  return(bufs)
  
}

clipped_polys <- foreach::foreach(i = 1:nrow(buffer_polys), .errorhandling = "remove", .combine = "rbind",.packages = c("sf", "dplyr")) %dopar% {
 
clp = st_intersection(buffer_polys[i,],fin_poly) %>%
 dplyr::select(layer,layer.1,geometry) %>%
 mutate(j_area = st_area(.))

  return(clp)
}
parallel::stopCluster(myCluster)
attributes(clipped_polys$j_area)=NULL
attributes(buffer_polys$i_area)=NULL
attributes(buffer_polys$di_area)=NULL