myCluster <- parallel::makeCluster(cores)
doParallel::registerDoParallel(myCluster)

coords <- foreach::foreach(i = 1:nrow(comps),
                           .errorhandling = "remove",
                           .combine = "rbind",
                           .packages = c("sf", "terra")) %dopar% {
  
  crds = data.frame(begin_lon = st_coordinates(st_as_sf(nodes_sp[comps[i,1],]))[1],
                    begin_lat = st_coordinates(st_as_sf(nodes_sp[comps[i,1],]))[2],
                    begin_id = nodes_sp[comps[i,1],]$layer,
                    end_lon = st_coordinates(st_as_sf(nodes_sp[comps[i,2],]))[1],
                    end_lat = st_coordinates(st_as_sf(nodes_sp[comps[i,2],]))[2],
                    end_id = nodes_sp[comps[i,2],]$layer)
  
  return(crds)
  
}
parallel::stopCluster(myCluster)