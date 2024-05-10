myCluster <- parallel::makeCluster(cores)
doParallel::registerDoParallel(myCluster)

buffer_polys <- foreach::foreach(i = 1:nrow(fin_poly), .errorhandling = "remove", .combine = "rbind",.packages = c("sf", "dplyr")) %dopar% {
  
  bufs = st_buffer(fin_poly[i,],dist=distance) %>%
    mutate(area = st_area(.))
  gc()
  return(bufs)
  
}

clipped_polys <- foreach::foreach(i = 1:nrow(buffer_polys), .errorhandling = "remove", .combine = "rbind",.packages = c("sf", "dplyr")) %dopar% {
  
  clp = st_intersection(buffer_polys[i,],fin_poly) %>%
    dplyr::select(layer,layer.1,geometry) %>%
    mutate(red_area = st_area(.))
  
  gc()
  
  return(clp)
}
parallel::stopCluster(myCluster)

attributes(clipped_polys$red_area)=NULL