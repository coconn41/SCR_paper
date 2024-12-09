g=igraph::graph_from_data_frame(numerator,directed=TRUE,vertices = NULL)
E(g)$weight = (numerator$product_prob)
myCluster <- parallel::makeCluster(cores)
doParallel::registerDoParallel(myCluster)
full_max_df <- foreach::foreach(a = 1:nrow(wmu_nodes),
                                .verbose = TRUE,
                                .errorhandling = "remove",
                                .combine = "rbind",
                                .packages = c("sf","raster","dplyr","terra","igraph","units")) %dopar% {
                                  i_ind=0
                                  list_i = shortest_paths(g,from=V(g)[a],to=V(g))
                                  for(b in 1:as.numeric(length(list_i$vpath))){
                                    plength = length(list_i$vpath[[b]])
                                    if(plength<=1){next}
                                    i_ind=i_ind+1
                                    for(c in 1:plength){
                                      if(c==plength){break}
                                      if(c==1){combos = data.frame(i=as.numeric(list_i$vpath[[b]][c]),
                                                                   j=as.numeric(list_i$vpath[[b]][c+1]))}
                                      if(c>1){combos2 = data.frame(i=as.numeric(list_i$vpath[[b]][c]),
                                                                   j=as.numeric(list_i$vpath[[b]][c+1]))
                                      combos = rbind(combos2,combos)}
                                    }
                                    if(i_ind==1){df=left_join(as.data.frame(combos),
                                                              as.data.frame(numerator),
                                                              join_by(i,j)) %>%
                                      summarize(max_distance=sum(distance)) %>%
                                      as.data.frame()
                                    df$i = as.numeric(list_i$vpath[[b]][1])
                                    df$j = as.numeric(list_i$vpath[[b]][plength])}
                                    if(i_ind>1){df2=left_join(as.data.frame(combos),
                                                              as.data.frame(numerator),
                                                              join_by(i,j)) %>%
                                      summarize(max_distance=sum(distance)) %>%
                                      as.data.frame() 
                                    df2$i = as.numeric(list_i$vpath[[b]][1])
                                    df2$j = as.numeric(list_i$vpath[[b]][plength])
                                    df=rbind(df2,df)}
                                  }
                                  return(df)
                                }
parallel::stopCluster(myCluster)
numerator = as.data.frame(numerator) %>%
  select(-max_distance) %>%
  left_join(.,
            as.data.frame(full_max_df),
            join_by(i,j)) %>%
  distinct()
numerator$product_prob = exp(-theta*numerator$max_distance)
numerator$act_numerator = numerator$i_area*numerator$j_area*numerator$product_prob
result_df = data.frame(UNIT = rep(x,4),
                       wmu_type = rep("normal",4),
                       index = c("PC_index_half",
                                 "PC_index_double",
                                 "ECA_half",
                                 "ECA_double"),
                       value = c(sum(numerator$act_numerator)/(sing_wmu$area^2),
                                 sum(numerator$act_numerator*2)/(sing_wmu$area^2),
                                 sqrt(sum(numerator$act_numerator)),
                                 sqrt(sum(numerator$act_numerator*2))))