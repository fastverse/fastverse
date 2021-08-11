.core_pkg <- c("data.table", "magrittr", "kit", "collapse", "matrixStats", "fst")
# Not needed anymore 
# core_unloaded <- function() .core_pkg[!paste0("package:", .core_pkg) %in% search()]

is_attached <- function(x) paste0("package:", x) %in% search()
is_installed <- function(x) vapply(x, requireNamespace, TRUE, quietly = TRUE)

ckeck_attached <- function(needed = TRUE) {
  pkg <- fastverse_packages(include.self = FALSE)
  attached <- is_attached(pkg)
  return(pkg[if(needed) !attached else attached])
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

fastverse_attach <- function(to_load, txt = "Attaching packages", onattach = FALSE) {
  if(length(to_load) == 0L) return(invisible()) 
  
  pv <- package_version("fastverse")
  msg(rule(left = txt, style.left = function(x) bold(text_col(x)),
           right = paste("fastverse", pv), 
           style.right = function(x) sub(pv, text_col(pv), sub("fastverse", kingsblue("fastverse"), x, fixed = TRUE), fixed = TRUE), 
           style.rule = TRUE), 
      startup = onattach)
  
  versions <- vapply(to_load, package_version, character(1L))
  packages <- paste0(kingsblue(tick), " ", magenta2(format(to_load)), " ", text_col(format(versions)))
  
  if(length(packages) %% 2L == 1L) packages <- append(packages, "")
  
  col1 <- seq_len(length(packages) / 2L)
  info <- paste0(packages[col1], "     ", packages[-col1])
  
  suppressPackageStartupMessages({
      lapply(to_load, same_library)
  })
  
  msg(paste(info, collapse = "\n"), startup = onattach) # cat(paste(info, collapse = "\n"))
  
  invisible()
}


.onAttach <- function(libname, pkgname) {
  
  needed <- ckeck_attached() 

  if(length(needed) == 0L) return()
  
  fastverse_attach(needed, onattach = TRUE)
  
  if(!isTRUE(getOption("fastverse.quiet")) && !"package:conflicted" %in% search()) {
    x <- fastverse_conflicts() 
    if(length(x)) msg(fastverse_conflict_message(x), startup = TRUE)
  }
  
}


#' Detach fastverse packages
#' 
#' Detaches \emph{fastverse} packages, removing them from the \code{\link{search}} path.
#' 
#' @param \dots comma-separated package names, quoted or unquoted, or vectors of package names. If left empty, all packages returned by \code{\link{fastverse_packages}} are detached. 
#' @param unload logical. \code{TRUE} also unloads the packages using \code{\link[=detach]{detach(name, unload = TRUE)}}.
#' @param include.self logical. \code{TRUE} also includes the \code{fastverse} package - only applicable if \code{\dots} is left empty.  
#' @param force logical. should a \emph{fastverse} package be detached / unloaded even though other attached packages depend on it?
#' @param session logical. \code{TRUE} also removes the packages from \code{options("fastverse.extend")}, so they will not be attached again with \code{library(fastverse)} in the current session. If \code{\dots} is left empty and \code{include.self = TRUE}, this will clear \bold{all} \emph{fastverse} options set for the session. 
#' @param permanent logical. if \code{\dots} are used to detach certain packages, \code{permament = TRUE} will disable them being loaded the next time the \emph{fastverse} is loaded. 
#' This is implemented via a config file saved to the package directory. Core \emph{fastverse} packages can also be detached in this way. To add a package again use \code{extend_fastverse(..., permanent = TRUE)}. The config file can be removed with \code{\link{fastverse_reset}}.
#' 
#' @returns This function returns nothing. Courtesy to CRAN maintainers, and because my name is not Hadley Wickham - who ostensibly is able to publish an entire \emph{tidyverse} package without these redundant statements. 
#' @seealso \code{\link{fastverse_extend}}, \code{\link{fastverse}}
#' @export
fastverse_detach <- function(..., unload = FALSE, force = FALSE, include.self = TRUE, 
                             session = FALSE, permanent = FALSE) {
  
  if(missing(...)) {
    loaded <- ckeck_attached(needed = FALSE)
    if(include.self) loaded <- c(loaded, "fastverse") # Could already be part of loaded ??
    if(session) {
      options(fastverse.extend = NULL)
      if(include.self) options(fastverse.quiet = NULL, 
                               fastverse.styling = NULL)
    }
  } else {
    pkg <- tryCatch(c(...), error = function(e) .c(...))
    if(!is.character(pkg) || length(pkg) > 200L) pkg <- .c(...)
    loaded <- pkg[is_attached(pkg)] # Include self? -> nope, not sensible...
    if(session) {
      epkg <- getOption("fastverse.extend")
      if(length(epkg)) options(fastverse.extend = if(length(pdiff <- setdiff(epkg, pkg))) pdiff else NULL)
    }
  }
  
  if(permanent) {
    if(missing(...)) stop("permanently detaching all packages in the fastverse does not make any sense. Please indicate packages to permanently detach.")
    ext_pkg_file <- gconf_path()
    fexl <- file.exists(ext_pkg_file)
    fileConn <- file(ext_pkg_file)
    ext_pkg <- if(fexl) setdiff(readLines(fileConn), pkg) else setdiff(.core_pkg, pkg)
    if(identical(ext_pkg, .core_pkg)) {
      close(fileConn)
      if(fexl) file.remove(ext_pkg_file) # If condition necessary, else error !!
    } else {
      writeLines(ext_pkg, fileConn)
      close(fileConn)
    }
  }
  
  if(length(loaded)) {
    ul <- paste0("package:", loaded) 
    for(i in ul)  eval(substitute(detach(pkgi, unload = unload, 
                                         character.only = TRUE, force = force), list(pkgi = i)))
  }
}


# Here make sure the order is such that they can be sequentially be detached by fastverse_detach() !!
topics_selector <- function(x) {
  switch(if(is.character(x)) toupper(x) else x, 
         TS = c("xts", "zoo", "roll"), 
         DT = c("lubridate", "clock", "anytime", "fasttime", "timechange"), # nanotime??
         ST = c("snakecase", "stringr", "stringi"),
         SC = c("Rfast", "Rfast2", "parallelDist", "coop"), # "fastmatch", "fastmap", (not on topic), "fastDummies" (16 dependencies)
         SP = c("stars", "terra", "sf"), # "sp" "rgdal" "raster"
         VI = c("dygraphs", "ggplot2", "scales", "lattice", "grid"), # "latticeExtra", "gridExtra", "gridtext", "plotly", "viridis" (32 dependencies), "RColorBrewer" (main function provided by scales)
         TV = c("tidytable", "tidyfast", "tidyfst", "tidyft", "maditr"), # "dtplyr", "table.express" import dplyr!!
         stop("Unknown topic:", x))
}

#' Extend the fastverse
#' 
#' Loads additional packages as part of the \emph{fastverse}. By default only for the session, but extensions can be saved up to reinstallation/updating of the \emph{fastverse} package. 
#' 
#' @param \dots comma-separated package names, quoted or unquoted, or vectors of package names. 
#' @param topics integer or character. Short-keys to attach groups of related and packages suggested as extensions to the \emph{fastverse} (not case sensitive if character). Unavailable packages are skipped unless \code{install = TRUE}.  
#' \enumerate{
#' \item \code{"TS"}: Time Series. Attaches \emph{xts}, \emph{zoo} and \emph{roll}. 
#' \item \code{"DT"}: Dates and Times. Attaches \emph{lubridate}, \emph{clock}, \emph{anytime}, \emph{fasttime} and \emph{timechange}.
#' \item \code{"ST"}: Strings. Attaches \emph{stringr}, \emph{stringi} and \emph{snakecase}.
#' \item \code{"SC"}: Statistics and Computing. Attaches \emph{Rfast}, \emph{Rfast2}, \emph{parallelDist} and \emph{coop}. % \emph{fastDummies}, 
#' \item \code{"SP"}: Spatial. Attaches \emph{sf}, \emph{stars} and \emph{terra}.
#' \item \code{"VI"}: Visualization. Attaches \emph{dygraphs}, \emph{ggplot2}, \emph{scales}, \emph{lattice} and \emph{grid}. % \emph{RColorBrewer} and \emph{viridis}.
#' \item \code{"TV"}: Tidyverse-Like. Attaches \emph{tidytable}, \emph{tidyfast}, \emph{tidyfst}, \emph{tidyft} and \emph{maditr}. % , \emph{table.express} and \emph{dtplyr}, import dplyr
#' }
#' @param install logical. Install packages not available?
#' @param permanent logical. Should packages be saved and included when \code{library(fastverse)} is called next time? Implemented via a config file saved to the package directory. The file will be removed if the \emph{fastverse} is reinstalled, and can be removed without reinstallation using \code{\link{fastverse_reset}}. Packages can be removed from the config file using \code{\link[=fastverse_detach]{fastverse_detach(..., permanent = TRUE)}}.
#' @param check.conflicts logical. Should conflicts between extension packages and attached packages be checked?
#' 
#' @returns This function returns nothing. Courtesy to CRAN maintainers, and because my name is not Hadley Wickham - who ostensibly is able to publish an entire \emph{tidyverse} package without these redundant statements. 
#' @details 
#' The \emph{fastverse} can be extended using a free choice of packages, packages listed under \code{topics}, or a combination of both. If \code{install = FALSE}, only packages 
#' among the \code{topics} groups that are available are considered, others are disregarded. 
#' 
#' When the \emph{fastverse} is extended calling \code{fastverse_extend(...)}, the packages that are not attached are attached, but conflicts are checked for all specified packages. 
#' If \code{permanent = FALSE}, an \code{options("fastverse.extend")} is set which stores these extension packages, regardless of whether they were already attached or not. When calling 
#' \code{\link{fastverse_packages}}, \code{\link{fastverse_deps}}, \code{\link{fastverse_conflicts}}, \code{\link{fastverse_update}}, \code{\link{fastverse_sitrep}} or \code{\link{fastverse_detach}}, these packages are included as part of the \emph{fastverse}. 
#' This is also the case if \code{permanent = TRUE}, with the only difference that instead of populating the option, a file is saved to the package directory such that the packages are also loaded
#' (as part of the core \emph{fastverse}) when calling \code{library(fastverse)} in the next session. To extend the \emph{fastverse} for the current session when it is not yet loaded, users can also set \code{options(fastverse.extend = c(...))}, where \code{c(...)}
#' is a character vector of package names, before calling \code{library(fastverse)}. 
#' 
#' @seealso \code{\link{fastverse_detach}}, \code{\link{fastverse}}
#' @export
#' @examples \donttest{
#' ex <- getOption("fastverse.extend")
#' fastverse_extend(xts, stringi)
#' fastverse_extend(fasttime, topics = "VI")
#' 
#' # Undoing this again
#' fastverse_detach(setdiff(getOption("fastverse.extend"), ex), session = TRUE)
#' rm(ex)
#' }
fastverse_extend <- function(..., topics = NULL, install = FALSE, permanent = FALSE, 
                             check.conflicts = !isTRUE(getOption("fastverse.quiet"))) {
  
  if(!missing(...)) {
    epkg <- tryCatch(c(...), error = function(e) .c(...))
    if(!is.character(epkg) || length(epkg) > 200L) epkg <- .c(...)
  }
  
  if(length(topics)) {
    tpkg <- unlist(lapply(topics, topics_selector)) # needed, don't use sapply
    if(!install) tpkg <- tpkg[is_installed(tpkg)]
    epkg <- if(missing(...)) tpkg else unique(c(epkg, tpkg))
  }
  
  if(missing(...) && is.null(topics)) stop("Need to either supply package names to ... or use the 'topics' argument to load groups of related packages")
  # Need to be able to first temporarily and then permanently extend fastverse. It cannot be that packages from a first temporary extension get added here if permanent = TRUE
  pkg <- fastverse_packages(extensions = !permanent, include.self = FALSE)
  
  if(permanent) {
    ext_pkg_file <- gconf_path()
    fileConn <- file(ext_pkg_file)
    writeLines(unique(c(pkg, epkg)), fileConn)
    close(fileConn)
  } else {
    if(length(ex <- getOption("fastverse.extend"))) { # It was already extended before
      options(fastverse.extend = unique(c(ex, setdiff(epkg, pkg))))
    } else { # It is extended for the first time
      options(fastverse.extend = setdiff(epkg, pkg))
    }
  }
  
  # epkg <- setdiff(epkg, pkg[is_attached(pkg)]) # Not clear what the point of this line is.
  needed <- epkg[!is_attached(epkg)]
  if(length(needed) == 0L) return(invisible())

  if(install) {
    inst <- needed[!is_installed(needed)]
    if(length(inst)) {
      install.packages(inst)
      cat("\n")
    }
  } 
  
  fastverse_attach(needed, "Attaching extension packages")

  if(check.conflicts && !"package:conflicted" %in% search()) {
    x <- fastverse_conflicts(epkg)
    if(length(x)) msg(fastverse_conflict_message(x))
  }
}


