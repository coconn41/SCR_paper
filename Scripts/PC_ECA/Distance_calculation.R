# Do the PC calculations
myCluster <- parallel::makeCluster(cores)
doParallel::registerDoParallel(myCluster)
numerator <- foreach::foreach(a = 1:nrow(combinations),
                              .verbose = F,
                              .errorhandling = "remove",
                              .combine = "rbind",
                              .packages = c("sf","raster","dplyr","terra","igraph","units")) %dopar% {
                                ipatch = wmu_nodes[combinations[a,1],]
                                jpatch = wmu_nodes[combinations[a,2],]
                                combodf = data.frame(i = combinations[a,1],
                                                     j = combinations[a,2],
                                                     i_area = ipatch$area,
                                                     j_area = jpatch$area,
                                                     distance = st_distance(ipatch,jpatch))
                                return(combodf)
                              }
parallel::stopCluster(myCluster)
unregister_dopar()
attributes(numerator$distance)=NULL
numerator$product_prob=exp(-.001788497*numerator$distance)
numerator$max_distance = NA
