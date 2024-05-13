myCluster <- parallel::makeCluster(cores)
doParallel::registerDoParallel(myCluster)

comps <- foreach::foreach(i = 1:nrow(fin_poly), .errorhandling = "remove", .combine = "rbind", .packages = c("sf", "terra")) %dopar% {
  comps1=t(st_is_within_distance(fin_poly[i,],fin_poly,dist=distance,sparse = F))
  comps1=which(comps1)
  dist_df = data.frame(row=rep(i,length(comps1)),
                       col=comps1)
  return(dist_df)
}

parallel::stopCluster(myCluster)

comps = comps %>%
  filter(row!=col)
comps2 = comps
fltr <- !duplicated(apply(comps, 1, function(x) paste0(sort(x), collapse = "")))
comps = comps[fltr, ]