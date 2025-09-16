myCluster <- parallel::makeCluster(cores)
doParallel::registerDoParallel(myCluster)

coords <- foreach::foreach(i = 1:nrow(comps), .errorhandling = "remove", .combine = "rbind", .packages = c("sf", "terra")) %dopar% {
  
  
  crds = data.frame(begin_lon = st_coordinates(st_as_sf(wmu_nodes[comps[i,1],]))[1],
                    begin_lat = st_coordinates(st_as_sf(wmu_nodes[comps[i,1],]))[2],
                    begin_id = wmu_nodes[comps[i,1],]$layer,
                    end_lon = st_coordinates(st_as_sf(wmu_nodes[comps[i,2],]))[1],
                    end_lat = st_coordinates(st_as_sf(wmu_nodes[comps[i,2],]))[2],
                    end_id = wmu_nodes[comps[i,2],]$layer)
  
  gc()
  
  return(crds)
  
}
parallel::stopCluster(myCluster)


begin.coords = coords[,c(1:3)]
names(begin.coords)=c("lon","lat","id")
end.coords = coords[,c(4:6)]
names(end.coords)=c("lon","lat","id")

l_sf2 <- vector("list", nrow(begin.coords))
for (i in seq_along(l_sf2)){
  l_sf2[[i]] <- st_linestring(as.matrix(rbind(begin.coords[i,], end.coords[i,])))
}


line_df = st_sfc(l_sf2) %>%
  st_as_sf() %>%
  st_set_crs(.,st_crs(wmus)) %>%
  mutate(origin_ID = begin.coords$id,
         destination_ID = end.coords$id,
         euclid_lengths = st_length(.))
attributes(line_df$euclid_lengths)=NULL
units(lcp_network$length)=NULL
sinuosity_df = line_df %>%
  st_drop_geometry() %>%
  left_join(.,lcp_network  %>%
              st_drop_geometry(),# %>% st_drop_geometry(),
              by=c("origin_ID","destination_ID")) %>%
  #dplyr::select(-direction),by=c("origin_ID","destination_ID")) %>%
  mutate(length = ifelse(is.na(length)==T,0,length),
         sinuosity = length/euclid_lengths,
         inv_sinuosity = ifelse(length==0,0,euclid_lengths/length)) %>%
  left_join(.,fin_poly,by=c("destination_ID"="layer")) %>%
  #mutate(mini_metric = inv_sinuosity*area) %>%
  dplyr::select(-area)

sinuosity_df2 = line_df %>%
  st_drop_geometry() %>%
  left_join(.,lcp_network, #%>% st_drop_geometry(),
            by=c("origin_ID","destination_ID")) %>%
  #dplyr::select(-direction),by=c("origin_ID","destination_ID")) %>%
  mutate(length = ifelse(is.na(length)==T,0,length),
         sinuosity = length/euclid_lengths,
         inv_sinuosity = ifelse(length==0,0,euclid_lengths/length)) %>%
  left_join(.,fin_poly,by=c("destination_ID"="layer")) %>%
  #mutate(mini_metric = inv_sinuosity*area) %>%
  dplyr::select(-area) %>%
  mutate(origin_ID2 = destination_ID,
         destination_ID2 = origin_ID) %>%
  mutate(origin_ID = origin_ID2,
         destination_ID = destination_ID2) %>%
  dplyr::select(-c(origin_ID2,destination_ID2,geometry.y)) %>%
  rename(geometry = 'geometry.x')

sinuosity_df = rbind(sinuosity_df,sinuosity_df2)

line_df2 = st_sfc(l_sf2) %>%
  st_as_sf() %>%
  st_set_crs(.,st_crs(wmus)) %>%
  mutate(origin_ID = end.coords$id,
         destination_ID = begin.coords$id,
         euclid_lengths = st_length(.))
attributes(line_df2$euclid_lengths)=NULL

line_df = rbind(line_df,line_df2)
