.core_pck <- c("data.table", "magrittr", "kit", "collapse", "matrixStats", "fst")
# core_unloaded <- function() .core_pck[!paste0("package:", .core_pck) %in% search()]
is_attached <- function(x) paste0("package:", x) %in% search()

ckeck_attached <- function(needed = TRUE) {
  pck <- fastverse_packages(include.self = FALSE)
  attached <- is_attached(pck)
  return(pck[if(needed) !attached else attached])
}

# Attach the package from the same package library it was
# loaded from before. https://github.com/fastverse/fastverse/issues/171
same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  do.call("library", list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE))
}

tick <- "v" # "\\U2713" # Heavy: \U2714 # cli::symbol$tick # Load from RDA file...

# col_align <- function(x) {
#   ncx <- nchar(x)
#   max_ncx <- max(ncx)
#   spaces <- vapply(max_ncx - ncx, function(x) paste(rep(" ", x), collapse = ""), character(1), USE.NAMES = FALSE)
#   paste0(x, spaces)
# }

fastverse_attach <- function(to_load, txt = "Attaching core packages", onattach = FALSE) {
  if(length(to_load) == 0L) return(invisible())
  
  msg(rule(left = bold(txt),
           right = paste0(kingsblue("fastverse "), package_version("fastverse"))), 
      startup = TRUE)
  
  versions <- vapply(to_load, package_version, character(1))
  packages <- paste0(kingsblue(tick), " ", magenta2(format(to_load)), " ", grey09(format(versions)))
  
  if(length(packages) %% 2L == 1L) packages <- append(packages, "")
  
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])
  
  msg(paste(info, collapse = "\n"), startup = TRUE) # cat(paste(info, collapse = "\n"))
  
  oldopts <- options(warn = -1)
  on.exit(options(oldopts))
  
  suppressPackageStartupMessages({
    if(any(to_load == "matrixStats")) {
      lapply(setdiff(to_load, "matrixStats"), same_library)
      replace_matrixStats()
    } else {
      lapply(to_load, same_library)
      # Special case: matrixStats was loaded before the fastverse
      if(onattach && any(fastverse_packages(include.self = FALSE) == "matrixStats") && is_attached("matrixStats")) {
          detach("package:matrixStats", unload = TRUE)
          replace_matrixStats()
      }
    }
  })
  
  invisible()
}

#' Detach fastverse packages
#' 
#' Detaches fastverse packages (removing them from the \code{\link{search}} path).
#' 
#' @param \dots comma-separated package names.
#' @param unload logical. \code{TRUE} also unloads the package using \code{\link[=detach]{detach(name, unload = TRUE)}}.
#' @param include.self logical. \code{TRUE} also includes the fastverse package. 
#' @param force logical. should a fastverse package be detached even though other attached packages depend on it?
#' @param permanent logical. if \code{\dots} are used to detach certain packages, \code{permament = TRUE} will disable them being loaded the next time the fastverse is loaded. 
#' This is implemented via a config file saved to the package directory. Core fastverse packages can also be detached in this way. To add a package again use \code{extend_fastverse(..., permanent = TRUE)}.
#' @export
#' @examples 
#' fastverse_detach()
fastverse_detach <- function(..., unload = TRUE, force = FALSE, include.self = TRUE, permanent = FALSE) {
  
  if(missing(...)) {
    loaded <- ckeck_attached(needed = FALSE)
    if(include.self) loaded <- c(loaded, "fastverse") # Could already be part of loaded ??
  } else {
    pck <- as.character(substitute(c(...))[-1L]) # TODO: Programmable? (i.e. character vector??)
    loaded <- pck[is_attached(pck)] # Include self? -> nope, not sensible...
  }
  
  if(permanent) {
    if(missing(...)) stop("permanently detaching all packages in the fastverse does not make any sense. Please indicate packages to permanently detach.")
    ext_pck_file <- paste(system.file(package = 'fastverse'), 'fastverse_ext_pck.txt', sep = '/')
    fileConn <- file(ext_pck_file)
    ext_pck <- if(file.exists(ext_pck_file)) setdiff(readLines(fileConn), pck) else setdiff(.core_pck, pck)
    writeLines(ext_pck, fileConn)
    close(fileConn)
  }
  
  if(length(loaded)) {
    ul <- paste0("package:", loaded) 
    for(i in ul)  eval(substitute(detach(pck, unload = unload, character.only = TRUE, force = force), list(pck = i)))
  }
}



topics_selector <- function(x) {
  switch(toupper(x), 
         TS = c("xts", "zoo", "roll"), 
         DT = c("lubridate", "clock", "fasttime"),
         SR = c("stringr", "stringi", "snakecase"),
         SC = c("Rfast", "Rfast2", "fastmatch", "fastmap", "fastDummies", "parallelDist", "coop"),
         SP = c("sf", "stars", "terra"), # "sp" "rgdal" "raster"
         VI = c("dygraphs", "lattice", "latticeExtra", "grid", "gridExtra", "ggplot2", "scales", "RColorBrewer", "viridis"), # "gridtext", "plotly"
         TV = c("dtplyr", "tidytable", "tidyfst", "tidyft", "tidyfast", "table.express", "maditr"))
}

#' Extend the fastverse with packages
#' 
#' Loads additional packages as part of the fastverse.
#' 
#' @param \dots comma-separated package names.
#' @param topics character. Short-keys to attach groups of related packages: 
#' @param install logical. Install packages not available?
#' @param permanent logical. Should extensions be saved and included when \code{library(fastverse)} is called next time. This is implemented via a config file saved to the package directory.
#' @export
#' @examples 
#' fastverse_extend(xts, roll)
fastverse_extend <- function(..., topics = NULL, install = FALSE, permanent = FALSE) {
  
  if(!missing(...)) epck <- as.character(substitute(c(...))[-1L]) # TODO: Programmable? (i.e. character vector??)
  if(length(topics) || install) inst_pck <- installed.packages()[, "Package"]
  
  if(length(topics)) {
    tpck <- sapply(topics, topics_selector)
    if(!install) tpck <- tpck[tpck %in% inst_pck]
    epck <- if(missing(...)) tpck else unique(c(epck, tpck))
  }
  
  pck <- fastverse_packages(include.self = FALSE)
  
  if(permanent) {
    ext_pck_file <- paste(system.file(package = 'fastverse'), 'fastverse_ext_pck.txt', sep = '/')
    fileConn <- file(ext_pck_file)
    writeLines(unique(c(pck, epck)), fileConn)
    close(fileConn)
  }
  
  epck <- setdiff(epck, pck[is_attached(pck)])
  needed <- epck[!is_attached(epck)]
  if(length(needed) == 0L) return(invisible())

  if(install) {
    inst <- epck[!(epck %in% inst_pck)]
    if(length(inst)) {
      install.packages(inst)
      cat("\n")
    }
  } 
  
  fastverse_attach(epck, "Attaching extension packages")

  if(!"package:conflicted" %in% search()) {
    x <- fastverse_conflicts(epck)
    msg(fastverse_conflict_message(x), startup = TRUE)
  }
}
