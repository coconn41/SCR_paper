graphab_links_fixed = function(proj_name, distance = "cost", name, cost = NULL, topo = "planar", 
          remcrosspath = FALSE, proj_path = NULL, alloc_ram = NULL) 
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
  if (!inherits(distance, "character")) {
    stop("'distance' must be a character string")
  }
  else if (!(distance %in% c("cost", "euclid"))) {
    stop("'distance' must be equal to 'cost' or 'euclid'")
  }
  if (!inherits(remcrosspath, "logical")) {
    stop("'remcrosspath' must be a logical.")
  }
  if (distance == "cost") {
    if (inherits(cost, "data.frame")) {
      if (!all(c("code", "cost") %in% colnames(cost))) {
        stop("The columns of cost must include 'code' and 'cost'")
      }
      else if (any(is.na(as.numeric(cost$code)))) {
        stop("'code' column must include numeric values")
      }
      else if (any(is.na(as.numeric(cost$cost)))) {
        stop("'cost' column must include numeric values")
      }
      if (inherits(cost$code, c("factor", "character"))) {
        cost$code <- as.numeric(as.character(cost$code))
      }
      if (inherits(cost$cost, c("factor", "character"))) {
        cost$cost <- as.numeric(as.character(cost$cost))
      }
      rast_codes <- graph4lg::get_graphab_raster_codes(proj_name = proj_name, 
                                                       mode = "all", proj_path = proj_path)
      if (!all(rast_codes %in% cost$code)) {
        stop("'code' column must include all the raster code values.")
      }
      ncode <- nrow(cost)
      vec_cost <- c()
      for (i in 1:ncode) {
        vec_cost <- c(vec_cost, paste0(cost[i, "code"], 
                                       "=", cost[i, "cost"]))
      }
    }
    else {
      if (inherits(cost, "character")) {
        if (stringr::str_sub(cost, start = -4L) == ".tif") {
          extcost <- cost
          if (!(file.exists(normalizePath(extcost, mustWork = FALSE)))) {
            stop(paste0(extcost, " must be an existing cost surface raster file ('.tif')"))
          }
        }
      }
      else {
        stop("'cost' must be a data.frame or a cost surface raster file ('.tif')")
      }
    }
  }
  else if (!is.null(cost)) {
    message("'cost' argument is ignored with 'distance = euclid'")
  }
  if (!inherits(name, "character")) {
    stop("'name' must be a character string")
  }
  gr <- get_graphab(res = FALSE, return = TRUE)
  if (gr == 1) {
    message("Graphab has been downloaded")
  }
  java.path <- Sys.which("java")
  if(cluster==TRUE){version <- "Graphab-3.0.5.jar"
  path_to_graphab <- normalizePath(file.path("/user/collinoc/", 
                                             version))
  path_to_graphab <- shQuote(path_to_graphab)}
  if(cluster==FALSE){version <- "graphab-2.8.jar"
  path_to_graphab <- normalizePath(file.path(rappdirs::user_data_dir(), 
                                             "graph4lg_jar", version))
  path_to_graphab <- shQuote(path_to_graphab)}
  cmd <- c("-Djava.awt.headless=true", "-jar", path_to_graphab, 
           "--project", proj_end_path, "--linkset", paste0("distance=", 
                                                           distance), paste0("name=", name))
  if (topo == "complete") {
    cmd <- c(cmd, "complete")
  }
  if (remcrosspath) {
    cmd <- c(cmd, "remcrosspath")
  }
  if (distance == "cost") {
    if (inherits(cost, "data.frame")) {
      cmd <- c(cmd, vec_cost)
    }
    else {
      cmd <- c(cmd, paste0("extcost=", extcost))
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
                             "/", name, "-links.shp"))) {
        message(paste0("Link set '", name, "' has been created in the project ", 
                       proj_name))
      }
      else {
        message("The link set creation did not succeed.")
      }
    }
  }
  else {
    if (file.exists(paste0(proj_path, "/", proj_name, "/", 
                           name, "-links.shp"))) {
      message(paste0("Link set '", name, "' has been created in the project ", 
                     proj_name))
    }
    else {
      message("The link set creation did not succeed.")
    }
  }
}
