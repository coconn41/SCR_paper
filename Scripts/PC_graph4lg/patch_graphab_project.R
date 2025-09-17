graphab_project_fixed <- function (proj_name, raster, habitat, nomerge = FALSE, minarea = 0, 
          nodata = NULL, maxsize = NULL, con8 = FALSE, alloc_ram = NULL, 
          proj_path = NULL) 
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
  if (!inherits(raster, "character")) {
    stop("'raster' must be a character string")
  }
  else if (stringr::str_sub(raster, -4, -1) != ".tif") {
    stop(paste0(raster, " must be a .tif raster file."))
  }
  else if (!(file.exists(normalizePath(raster, mustWork = FALSE)))) {
    stop(paste0(normalizePath(raster, mustWork = FALSE), 
                " must be an existing .tif raster file."))
  }
  if (!inherits(habitat, c("numeric", "integer"))) {
    stop("'habitat' must be an integer indicating the habitat code")
  }
  if (!inherits(nomerge, c("logical"))) {
    stop(paste0("'nomerge' must be a logical indicating whether ", 
                "contiguous patches are not merged."))
  }
  if (!inherits(minarea, c("numeric", "integer"))) {
    stop("'minarea' must be an integer indicating minimum patch size")
  }
  if (!is.null(maxsize)) {
    if (!inherits(maxsize, c("numeric", "integer"))) {
      stop(paste0("'maxsize' must be an integer indicating maximum side length", 
                  " of the rectangular extent of every habitat patch"))
    }
  }
  if (!inherits(con8, c("logical"))) {
    stop(paste0("'con8' must be a logical indicating whether a neighboorhood of ", 
                " 8 pixels is used for patch definition (if TRUE). ", 
                "Default=FALSE: 4 pixel neighboorhood."))
  }
  gr <- get_graphab(res = FALSE, return = TRUE)
  if (gr == 1) {
    message("Graphab has been downloaded")
  }
  java.path <- Sys.which("java")
  if(cluster==TRUE){version <- "Graphab-3.0.5.jar"}
  if(cluster==FALSE){version <- "graphab-2.8.jar"}
  path_to_graphab <- normalizePath(file.path(rappdirs::user_data_dir(), 
                                             "graph4lg_jar", version))
  path_to_graphab <- shQuote(path_to_graphab)
  cmd <- c("-Djava.awt.headless=true", "-jar", path_to_graphab, 
           "--create", proj_name, raster, paste0("habitat=", paste(habitat, 
                                                                   collapse = ",")))
  if (nomerge) {
    cmd <- c(cmd, "nomerge")
    message("Be careful, the nomerge = TRUE option is in development.\n            We cannot guarantee the results are correct.")
  }
  if (!is.null(nodata)) {
    cmd <- c(cmd, paste0("nodata=", nodata))
  }
  cmd <- c(cmd, paste0("minarea=", minarea))
  if (!is.null(maxsize)) {
    cmd <- c(cmd, paste0("maxsize=", maxsize))
  }
  if (con8) {
    cmd <- c(cmd, "con8")
  }
  cmd <- c(cmd, paste0("dir=", proj_path))
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
                             "/", proj_name, ".xml"))) {
        message(paste0("Graphab project ", proj_name, 
                       " has been created in directory: ", proj_path))
      }
      else {
        message("The project creation did not succeed.")
      }
    }
  }
  else {
    if (file.exists(paste0(proj_path, "/", proj_name, "/", 
                           proj_name, ".xml"))) {
      message(paste0("Graphab project ", proj_name, " has been created in directory: ", 
                     proj_path))
    }
    else {
      message("The project creation did not succeed.")
    }
  }
}
