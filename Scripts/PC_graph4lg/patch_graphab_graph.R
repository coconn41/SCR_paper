graphab_graph_fixed <- function(proj_name, linkset = NULL, name = NULL, thr = NULL, 
          cost_conv = FALSE, proj_path = NULL, alloc_ram = NULL) 
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
  if (!is.null(linkset)) {
    if (!inherits(linkset, "character")) {
      stop("'linkset' must be a character string")
    }
    else if (!(paste0(linkset, "-links.csv") %in% list.files(path = paste0(proj_path, 
                                                                           "/", proj_name)))) {
      stop("The linkset you refer to does not exist.\n           Please use graphab_link() before.")
    }
  }
  else if (length(list.files(path = paste0(proj_path, "/", 
                                           proj_name), pattern = "-links.csv")) == 0) {
    stop("There is not any linkset in the project you refer to.\n         Please use graphab_link() before.")
  }
  else {
    ngraph <- length(list.files(path = paste0(proj_path, 
                                              "/", proj_name), pattern = "-links.csv"))
    message(paste0(ngraph, " graph(s) will be created"))
  }
  if (!is.null(name)) {
    if (!inherits(name, "character")) {
      stop("'name' must be a character string")
    }
  }
  else if (ngraph > 1) {
    stop("You cannot specify a graph name when more than one graph is created")
  }
  if (!is.null(thr)) {
    if (!inherits(thr, c("numeric", "integer"))) {
      stop("'thr' must be a numeric or an integer threshold value.")
    }
  }
  if (!is.logical(cost_conv)) {
    stop("'cost_conv' must be a logical (TRUE or FALSE).")
  }
  gr <- get_graphab(res = FALSE, return = TRUE)
  if (gr == 1) {
    message("Graphab has been downloaded")
  }
  java.path <- Sys.which("java")
  if(cluster==TRUE){version <- "Graphab-3.0.5.jar"
  path_to_graphab <- normalizePath(file.path("/user/collinoc", 
                                             version))
  path_to_graphab <- shQuote(path_to_graphab)}
  if(cluster==FALSE){version <- "graphab-2.8.jar"
  path_to_graphab <- normalizePath(file.path(rappdirs::user_data_dir(), 
                                             "graph4lg_jar", version))
  path_to_graphab <- shQuote(path_to_graphab)}
  cmd <- c("-Djava.awt.headless=true", "-jar", path_to_graphab, 
           "--project", proj_end_path)
  if (!is.null(linkset)) {
    cmd <- c(cmd, "--uselinkset", linkset)
  }
  cmd <- c(cmd, "--graph")
  if (!is.null(name)) {
    cmd <- c(cmd, paste0("name=", name))
  }
  if (!is.null(thr)) {
    if (cost_conv) {
      cmd <- c(cmd, paste0("threshold={", thr, "}"))
    }
    else {
      cmd <- c(cmd, paste0("threshold=", thr))
    }
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
      if (file.exists(paste0(proj_path, "/", proj_name, 
                             "/", name, "-voronoi.shp"))) {
        message(paste0("Graph '", name, "' has been created in the project ", 
                       proj_name))
      }
      else {
        message("The graph creation did not succeed.")
      }
    }
  }
  else {
    if (file.exists(paste0(proj_path, "/", proj_name, "/", 
                           name, "-voronoi.shp"))) {
      message(paste0("Graph '", name, "' has been created in the project ", 
                     proj_name))
    }
    else {
      message("The graph creation did not succeed.")
    }
  }
}
