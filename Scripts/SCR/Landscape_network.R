myCluster <- parallel::makeCluster(cores)
doParallel::registerDoParallel(myCluster)

lcp_network <- foreach::foreach(i = 1:nrow(comps), .errorhandling = "remove", .combine = "rbind", .packages = c("sf","raster","gdistance","tmaptools","dplyr","leastcostpath","terra")) %dopar% {
  
  bbdf <- sf::st_bbox(fin_poly[c(comps[i,1],comps[i,2]),]) %>%
    tmaptools::bb_poly(.,projection = sf::st_crs(fin_poly)) %>%
    sf::st_as_sf()
  
  tr1=leastcostpath::create_cs(terra::crop(x=Rgrid %>%
                                             terra::rast(),
                                           y = bbdf,
                                           mask=T))
  
  lcp <- leastcostpath::create_lcp(x = tr1,
                                   origin = wmu_nodes[comps[i,1],,drop=FALSE],
                                   destination = wmu_nodes[comps[i,2],, drop=FALSE]) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(length = sf::st_length(.),
                  origin_ID = fin_poly[comps[i,1],]$layer,
                  destination_ID =fin_poly[comps[i,2],]$layer) #lcp$origin_ID <- comps[i,1]
  
  return(lcp)
}

parallel::stopCluster(myCluster)

attributes(lcp_network$length) <- NULL
lcp_network=lcp_network[,-c(1:3)]
