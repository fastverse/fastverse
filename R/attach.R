.core_pck <- c("data.table", "magrittr", "kit", "collapse", "matrixStats", "fst")
# Not needed anymore 
# core_unloaded <- function() .core_pck[!paste0("package:", .core_pck) %in% search()]

is_attached <- function(x) paste0("package:", x) %in% search()

ckeck_attached <- function(needed = TRUE) {
  pck <- fastverse_packages(include.self = FALSE)
  attached <- is_attached(pck)
  return(pck[if(needed) !attached else attached])
}

# Attach the package from the same package library it was
# loaded from before. https://github.com/tidyverse/tidyverse/issues/171
same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  do.call("library", list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE))
}

tick <- "v" # "\\U2713" # Heavy: \U2714 # cli::symbol$tick

# col_align <- function(x) {
#   ncx <- nchar(x)
#   max_ncx <- max(ncx)
#   spaces <- vapply(max_ncx - ncx, function(x) paste(rep(" ", x), collapse = ""), character(1), USE.NAMES = FALSE)
#   paste0(x, spaces)
# }

fastverse_attach <- function(to_load, txt = "Attaching core packages", onattach = FALSE) {
  if(length(to_load) == 0L) return(invisible())
  
  msg(rule(left = txt, style.left = bold,
           right = paste("fastverse", package_version("fastverse")), 
           style.right = function(x) sub("fastverse", kingsblue("fastverse"), x, fixed = TRUE)), 
      startup = onattach)
  
  versions <- vapply(to_load, package_version, character(1))
  packages <- paste0(kingsblue(tick), " ", magenta2(format(to_load)), " ", grey09(format(versions)))
  
  if(length(packages) %% 2L == 1L) packages <- append(packages, "")
  
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])
  
  msg(paste(info, collapse = "\n"), startup = onattach) # cat(paste(info, collapse = "\n"))
  
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


.onAttach <- function(libname, pkgname) {
  
  needed <- ckeck_attached()
  
  if(length(needed) == 0L) return()
  
  fastverse_attach(needed, onattach = TRUE)
  
  if(!"package:conflicted" %in% search()) {
    x <- fastverse_conflicts() 
    if(length(x)) msg(fastverse_conflict_message(x), startup = TRUE)
  }
  
}


#' Detach \emph{fastverse} packages
#' 
#' Detaches \emph{fastverse} packages (removing them from the \code{\link{search}} path).
#' 
#' @param \dots comma-separated package names, quoted or unquoted, or vectors of package names. The code simply captures the \code{\dots} expression, evaluates it inside \code{\link{tryCatch}}, and if it fails coerces it to character. If left empty, all packages returned by \code{\link{fastverse_packages}} are detached. 
#' @param unload logical. \code{TRUE} also unloads the packages using \code{\link[=detach]{detach(name, unload = TRUE)}}.
#' @param include.self logical. \code{TRUE} also includes the fastverse package - only applicable if \code{\dots} is left empty.  
#' @param force logical. should a fastverse package be detached / unloaded even though other attached packages depend on it?
#' @param permanent logical. if \code{\dots} are used to detach certain packages, \code{permament = TRUE} will disable them being loaded the next time the fastverse is loaded. 
#' This is implemented via a config file saved to the package directory. Core \emph{fastverse} packages can also be detached in this way. To add a package again use \code{extend_fastverse(..., permanent = TRUE)}.
#' @export
#' @examples 
#' \dontrun{
#' fastverse_detach()
#' }
fastverse_detach <- function(..., unload = FALSE, force = FALSE, include.self = TRUE, permanent = FALSE) {
  
  if(missing(...)) {
    loaded <- ckeck_attached(needed = FALSE)
    if(include.self) loaded <- c(loaded, "fastverse") # Could already be part of loaded ??
  } else {
    ex <- substitute(c(...))
    pck <- tryCatch(eval(ex), error = function(e) as.character(ex[-1L]))
    if(!is.character(pck)) pck <- as.character(ex[-1L])
    loaded <- pck[is_attached(pck)] # Include self? -> nope, not sensible...
  }
  
  options(fastverse_extend = NULL)
  
  if(permanent) {
    if(missing(...)) stop("permanently detaching all packages in the fastverse does not make any sense. Please indicate packages to permanently detach.")
    ext_pck_file <- paste(system.file(package = 'fastverse'), 'fastverse_ext_pck.txt', sep = '/')
    fileConn <- file(ext_pck_file)
    fexl <- file.exists(ext_pck_file)
    ext_pck <- if(fexl) setdiff(readLines(fileConn), pck) else setdiff(.core_pck, pck)
    if(identical(ext_pck, .core_pck)) {
      close(fileConn)
      file.remove(ext_pck_file)
    } else {
      writeLines(ext_pck, fileConn)
      close(fileConn)
    }
  }
  
  if(length(loaded)) {
    ul <- paste0("package:", loaded) 
    for(i in ul)  eval(substitute(detach(pck, unload = unload, 
                                         character.only = TRUE, force = force), list(pck = i)))
  }
}


# Here make sure the order is such that they can be sequentially be detached by fastverse_detach() !!
topics_selector <- function(x) {
  switch(if(is.character(x)) toupper(x) else x, 
         TS = c("xts", "zoo", "roll"), 
         DT = c("lubridate", "clock", "timechange", "fasttime", "nanotime"),
         ST = c("snakecase", "stringr", "stringi"),
         SC = c("Rfast", "Rfast2", "fastDummies", "parallelDist", "coop"), # "fastmatch", "fastmap", 
         SP = c("stars", "terra", "sf"), # "sp" "rgdal" "raster"
         VI = c("dygraphs", "latticeExtra", "lattice", "gridExtra", "grid", "ggplot2", "scales", "RColorBrewer", "viridis"), # "gridtext", "plotly"
         TV = c("dtplyr", "tidytable", "tidyfst", "tidyft", "tidyfast", "table.express", "maditr"), 
         stop("Unknown topic:", x))
}

#' Extend the \emph{fastverse}
#' 
#' Loads additional packages as part of the \emph{fastverse}, either for the current session or permanently.
#' 
#' @param \dots comma-separated package names, quoted or unquoted, or vectors of package names. The code simply captures the \code{\dots} expression, evaluates it inside \code{\link{tryCatch}}, and if it fails coerces it to character.  
#' @param topics integer or character. Short-keys to attach groups of related and packages suggested as extensions to the \emph{fastverse} (not case sensitive if character). Packages that are not available are not attached unless \code{install = TRUE}.  
#' \enumerate{
#' \item \code{"TS"}: Time Series. Attaches \emph{xts}, \emph{zoo} and \emph{roll}. 
#' \item \code{"DT"}: Dates and Times. Attaches \emph{lubridate}, \emph{clock}, \emph{timechange}, \emph{fasttime} and \emph{nanotime}.
#' \item \code{"ST"}: Strings. Attaches \emph{stringr}, \emph{sringi} and \emph{snakecase}.
#' \item \code{"SC"}: Statistics and Computing. Attaches \emph{Rfast}, \emph{Rfast2}, \emph{fastDummies}, \emph{parallelDist} and \emph{coop}.
#' \item \code{"SP"}: Spatial. Attaches \emph{sf}, \emph{stars} and \emph{terra}.
#' \item \code{"VI"}: Visualization. Attaches \emph{dygraphs}, \emph{lattice}, \emph{latticeExtra}, \emph{grid}, \emph{gridExtra}, \emph{ggplot2}, \emph{scales}, \emph{RColorBrewer} and \emph{viridis}.
#' \item \code{"TV"}: Tidyverse-Like. Attaches \emph{dtplyr}, \emph{tidytable}, \emph{tidyfst}, \emph{tidyft}, \emph{tidyfast}, \emph{table.express} and \emph{maditr}.
#' }
#' @param install logical. Install packages not available?
#' @param permanent logical. Should extensions be saved and included when \code{library(fastverse)} is called next time. This is implemented via a config file saved to the package directory. The config file will be removed if the \emph{fastverse} is reinstalled. Packages can be removed from the config file using \code{\link[=fastverse_detach]{fastverse_detach(..., permanent = TRUE)}}.
#' @param check.conflicts logical. Should conflicts between extension packages and loaded packages be checked?
#' 
#' @details 
#' The \emph{fastverse} can be extended using a free choice of packages, packages listed under \code{topics}, or a combination of both. If \code{install = FALSE}, only packages 
#' among the \code{topics} groups that are available are considered, others are disregarded. 
#' 
#' When the \emph{fastverse} is extended calling \code{fastverse_extend(...)}, the packages that are not attached are attached and conflicts are checked for the newly attached packages. 
#' If \code{permanent = FALSE}, an \code{option("fastverse_extend")} is set which stores these extension packages, regardless of whether they were already attached or not. When calling 
#' \code{\link{fastverse_packages}}, \code{\link{fastverse_deps}}, \code{\link{fastverse_conflicts}}, \code{\link{fastverse_update}} or \code{\link{fastverse_detach}}, these packages are included as part of the fastverse. 
#' This is also the case if \code{permanent = TRUE}, with the only difference that instead of an option, a file is saved to the package directory such that the extended packages are also loaded
#' when calling \code{library(fastverse)} in the next session. To extend the \emph{fastverse} for the current session when it is not yet loaded, users can also set \code{options(fastverse_extend = c(...))}, where \code{c(...)}
#' is a character vector of packages, before calling \code{library(fastverse)}. 
#' 
#' @export
#' @examples 
#' fastverse_extend(Rfast, xts, stringi)
#' fastverse_extend(clock, topics = "TS")
fastverse_extend <- function(..., topics = NULL, install = FALSE, permanent = FALSE, 
                             check.conflicts = TRUE) {
  
  if(!missing(...)) {
    ex <- substitute(c(...))
    epck <- tryCatch(eval(ex), error = function(e) as.character(ex[-1L]))
    if(!is.character(epck)) epck <- as.character(ex[-1L])
  }
  if(length(topics) || install) inst_pck <- installed.packages()[, "Package"]
  
  if(length(topics)) {
    tpck <- unlist(lapply(topics, topics_selector)) # needed, don't use sapply
    if(!install) tpck <- tpck[tpck %in% inst_pck]
    epck <- if(missing(...)) tpck else unique(c(epck, tpck))
  }
  
  pck <- fastverse_packages(include.self = FALSE)
  
  if(permanent) {
    ext_pck_file <- paste(system.file(package = 'fastverse'), 'fastverse_ext_pck.txt', sep = '/')
    fileConn <- file(ext_pck_file)
    writeLines(unique(c(pck, epck)), fileConn)
    close(fileConn)
  } else {
    if(length(ex <- options("fastverse_extend")[[1L]])) { # It was already extended before
      options(fastverse_extend = unique(c(ex, setdiff(epck, pck))))
    } else { # It is extended for the first time
      options(fastverse_extend = setdiff(epck, pck))
    }
  }
  
  # epck <- setdiff(epck, pck[is_attached(pck)]) # Not clear what the point of this line is.
  needed <- epck[!is_attached(epck)]
  if(length(needed) == 0L) return(invisible())

  if(install) {
    inst <- needed[!(needed %in% inst_pck)]
    if(length(inst)) {
      install.packages(inst)
      cat("\n")
    }
  } 
  
  fastverse_attach(needed, "Attaching extension packages")

  if(check.conflicts && !"package:conflicted" %in% search()) {
    x <- fastverse_conflicts(epck)
    if(length(x)) msg(fastverse_conflict_message(x))
  }
}


