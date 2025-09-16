graphab_metric_fixed <- function(proj_name, graph, metric, multihab = FALSE, dist = NULL, 
          prob = 0.05, beta = 1, cost_conv = FALSE, return_val = TRUE, 
          proj_path = NULL, alloc_ram = NULL) 
{
  if (!is.null(proj_path)) {
    if (!dir.exists(proj_path)) {
      stop(paste0(proj_path, " is not an existing directory or the path is ", 
                  "incorrectly specified."))
    }
    else {
      proj_path <- normalizePath(proj_path)
    }
  }
  else {
    proj_path <- normalizePath(getwd())
  }
  if (!inherits(proj_name, "character")) {
    stop("'proj_name' must be a character string")
  }
  else if (!(paste0(proj_name, ".xml") %in% list.files(path = paste0(proj_path, 
                                                                     "/", proj_name)))) {
    stop("The project you refer to does not exist.\n         Please use graphab_project() before.")
  }
  proj_end_path <- paste0(proj_path, "/", proj_name, "/", proj_name, 
                          ".xml")
  if (!inherits(multihab, "logical")) {
    stop("'multihab' must be equal to either TRUE or FALSE.")
  }
  else if (multihab) {
    if (check_merge(proj_end_path)) {
      stop(paste0("The project must have been built without merging habitat ", 
                  "patches corresponding to different codes."))
    }
    message("Be careful, the multihab = TRUE option is in development.\n            We cannot guarantee the results are correct.")
    if (!any(metric %in% c("EC", "F", "IF", "BC"))) {
      stop(paste0("When 'multihab = TRUE', 'metric' must be equal to either ", 
                  "'EC', 'F', 'IF' or 'BC'."))
    }
    else {
      metric <- paste0(metric, "h")
    }
  }
  if (!inherits(graph, "character")) {
    stop("'graph' must be a character string")
  }
  else if (!(paste0(graph, "-voronoi.shp") %in% list.files(path = paste0(proj_path, 
                                                                         "/", proj_name)))) {
    stop("The graph you refer to does not exist")
  }
  else if (length(list.files(path = paste0(proj_path, "/", 
                                           proj_name), pattern = "-voronoi.shp")) == 0) {
    stop("There is not any graph in the project you refer to.\n         Please use graphab_graph() before.")
  }
  list_all_metrics <- c("PC", "EC", "ECh", "IIC", "dPC", "F", 
                        "BC", "IF", "Dg", "CCe", "CF", "Fh", "IFh", "BCh")
  list_glob_metrics <- list_all_metrics[1:5]
  list_loc_metrics <- list_all_metrics[6:length(list_all_metrics)]
  list_dist_metrics <- c("PC", "EC", "ECh", "F", "Fh", "BC", 
                         "BCh", "IF", "IFh", "dPC")
  if (metric %in% list_dist_metrics) {
    if (is.null(dist)) {
      stop(paste0("To compute ", metric, ", specify a distance associated to\n                  a dispersal probability (default=0.05)"))
    }
    else if (!inherits(dist, c("numeric", "integer"))) {
      stop("'dist' must be a numeric or integer value")
    }
    else if (!inherits(prob, c("numeric", "integer"))) {
      stop("'prob' must be a numeric or integer value")
    }
    else if (!inherits(beta, c("numeric", "integer"))) {
      stop("'beta' must be a numeric or integer value")
    }
    else if (beta < 0 || beta > 1) {
      stop("'beta' must be between 0 and 1")
    }
    else if (prob < 0 || prob > 1) {
      stop("'prob' must be between 0 and 1")
    }
  }
  if (metric == "dPC") {
    if (cost_conv) {
      stop("Option 'cost_conv = TRUE' is not available with the metric dPC")
    }
  }
  if (metric == "CF") {
    if (beta < 0 || beta > 1) {
      stop("'beta' must be between 0 and 1")
    }
  }
  if (!inherits(metric, "character")) {
    stop("'metric' must be a character string")
  }
  else if (!(metric %in% list_all_metrics)) {
    stop(paste0("'metric' must be ", paste(list_all_metrics, 
                                           collapse = " or ")))
  }
  else if (metric %in% list_loc_metrics) {
    level <- "patch"
  }
  else if (metric %in% list_glob_metrics) {
    level <- "graph"
  }
  if (!is.logical(cost_conv)) {
    stop("'cost_conv' must be a logical (TRUE or FALSE).")
  }
  if (!is.logical(return_val)) {
    stop("'return_val' must be a logical (TRUE or FALSE).")
  }
  gr <- get_graphab(res = FALSE, return = TRUE)
  if (gr == 1) {
    message("Graphab has been downloaded")
  }
  java.path <- Sys.which("java")
  version <- "graphab-2.8.jar"
  path_to_graphab <- normalizePath(file.path(rappdirs::user_data_dir(), 
                                             "graph4lg_jar", version))
  path_to_graphab <- shQuote(path_to_graphab)
  cmd <- c("-Djava.awt.headless=true", "-jar", path_to_graphab, 
           "--project", proj_end_path, "--usegraph", graph)
  if (level == "graph") {
    cmd <- c(cmd, "--gmetric", metric)
  }
  else if (level == "patch") {
    cmd <- c(cmd, "--lmetric", metric)
  }
  if (metric %in% list_dist_metrics) {
    if (cost_conv) {
      cmd <- c(cmd, paste0("d={", dist, "}"), paste0("p=", 
                                                     prob), paste0("beta=", beta))
    }
    else {
      cmd <- c(cmd, paste0("d=", dist), paste0("p=", prob), 
               paste0("beta=", beta))
    }
  }
  else if (metric == "CF") {
    cmd <- c(cmd, paste0("beta=", beta))
  }
  if (metric == "dPC") {
    cmd[8] <- "--delta"
    cmd[13] <- "obj=patch"
  }
  if (!is.null(alloc_ram)) {
    if (inherits(alloc_ram, c("integer", "numeric"))) {
      cmd <- c(paste0("-Xmx", alloc_ram, "g"), cmd)
    }
    else {
      stop("'alloc_ram' must be a numeric or an integer")
    }
  }
  rs <- system2(java.path, args = cmd, stdout = TRUE)
  if (length(rs) == 1) {
    if (rs == 1) {
      message("An error occurred")
    }
    else {
      message(paste0("Metric '", metric, "' has been computed in the project ", 
                     proj_name))
    }
  }
  else {
    message(paste0("Metric '", metric, "' has been computed in the project ", 
                   proj_name))
  }
  if (return_val) {
    if (level == "graph") {
      if (metric == "dPC") {
        name_txt <- paste0("delta-dPC_d", dist, "_p", 
                           prob, "_", graph, ".txt")
        res_dpc <- utils::read.table(file = paste0(proj_path, 
                                                   "/", proj_name, "/", name_txt), header = TRUE)[-1, 
                                                   ]
        vec_res <- c(paste0("Project : ", proj_name), 
                     paste0("Graph : ", graph), paste0("Metric : ", 
                                                       metric), paste0("Dist : ", dist), paste0("Prob : ", 
                                                                                                prob), paste0("Beta : ", beta))
        res <- list(vec_res, res_dpc)
      }
      else if (metric %in% c("EC", "PC", "IIC", "ECh")) {
        name_txt <- paste0(metric, ".txt")
        res_val <- utils::read.table(file = paste0(proj_path, 
                                                   "/", proj_name, "/", name_txt), header = TRUE)
        if (metric == "ECh") {
          colnames(res_val)[5:ncol(res_val)] <- paste0("EC_", 
                                                       stringr::str_sub(colnames(res_val)[5:ncol(res_val)], 
                                                                        2, -1))
        }
        vec_res <- c(paste0("Project : ", proj_name), 
                     paste0("Graph : ", graph), paste0("Metric : ", 
                                                       metric), paste0("Dist : ", dist), paste0("Prob : ", 
                                                                                                prob), paste0("Beta : ", beta))
        res <- list(vec_res, res_val)
      }
    }
    else if (level == "patch") {
      tab <- utils::read.csv(file = paste0(proj_path, "/", 
                                           proj_name, "/patches.csv"))
      if (metric %in% list_dist_metrics) {
        if (multihab == FALSE) {
          df_res <- tab[, c(1:3, ncol(tab))]
          vec_res <- c(paste0("Project : ", proj_name), 
                       paste0("Graph : ", graph), paste0("Metric : ", 
                                                         metric), paste0("Dist : ", dist), paste0("Prob : ", 
                                                                                                  prob), paste0("Beta : ", beta))
        }
        else {
          hab_code <- get_graphab_raster_codes(proj_name = proj_name, 
                                               mode = "habitat", proj_path = proj_path)
          nb_hab_type <- length(hab_code)
          if (metric %in% c("Fh", "IFh")) {
            df_res <- tab[, c(1:3, which(colnames(tab) == 
                                           "Code"), (ncol(tab) - nb_hab_type + 1):ncol(tab))]
            vec_res <- c(paste0("Project : ", proj_name), 
                         paste0("Graph : ", graph), paste0("Metric : ", 
                                                           metric), paste0("Dist : ", dist), paste0("Prob : ", 
                                                                                                    prob), paste0("Beta : ", beta))
          }
          else if (metric == "BCh") {
            nb_combin <- (nb_hab_type * (nb_hab_type - 
                                           1)/2) + nb_hab_type
            df_res <- tab[, c(1:3, which(colnames(tab) == 
                                           "Code"), (ncol(tab) - nb_combin + 1):ncol(tab))]
            vec_res <- c(paste0("Project : ", proj_name), 
                         paste0("Graph : ", graph), paste0("Metric : ", 
                                                           metric), paste0("Dist : ", dist), paste0("Prob : ", 
                                                                                                    prob), paste0("Beta : ", beta))
          }
        }
      }
      else if (metric == "CF") {
        name_met <- paste0(metric, "_beta", beta, "_", 
                           graph)
        df_res <- tab[, c(1:3, which(colnames(tab) == 
                                       name_met))]
        vec_res <- c(paste0("Project : ", proj_name), 
                     paste0("Graph : ", graph), paste0("Metric : ", 
                                                       metric), paste0("Beta : ", beta))
      }
      res <- list(vec_res, df_res)
    }
    return(res)
  }
}
