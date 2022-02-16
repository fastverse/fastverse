.core_pkg <- "lattice"

ckeck_attached <- function(needed = TRUE) {
  pkg <- fastversechild_packages(include.self = FALSE)
  attached <- is_attached(pkg)
  return(pkg[if(needed) !attached else attached])
}

# Attach the package from the same package library it was
# loaded from before. https://github.com/tidyverse/tidyverse/issues/171
same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  do.call("library", list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE))
}

tick <- "v" 

fastversechild_attach <- function(to_load, txt = "Attaching packages", onattach = FALSE) {
  if(length(to_load) == 0L) return(invisible()) 
  
  pv <- package_version("fastversechild")
  msg(rule(left = txt, style.left = function(x) bold(text_col(x)),
           right = paste("fastversechild", pv), 
           style.right = function(x) sub(pv, text_col(pv), sub("fastversechild", kingsblue("fastversechild"), x, fixed = TRUE), fixed = TRUE), 
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
  
  msg(paste(info, collapse = "\n"), startup = onattach)
  
  invisible()
}


.onAttach <- function(libname, pkgname) {
  
  pconfl <- file.exists(".fastversechild")
  
  if(pconfl) {
    popts <- project_options()
    if(length(popts$before)) popts$before()
  }
  
  needed <- ckeck_attached() 
  
  if(length(needed) > 0L) fastversechild_attach(needed, onattach = TRUE)
  
  if(length(needed) > 0L && !isTRUE(getOption("fastversechild.quiet")) && !"package:conflicted" %in% search()) {
    x <- fastversechild_conflicts() 
    if(length(x)) msg(fastversechild_conflict_message(x), startup = TRUE)
  }
  
  if(pconfl && length(popts$after)) popts$after()
  
}


#' Detach fastversechild packages
#' 
#' Detaches \emph{fastversechild} packages, removing them from the \code{\link{search}} path.
#' 
#' @param \dots comma-separated package names, quoted or unquoted, or vectors of package names. If left empty, all packages returned by \code{\link{fastversechild_packages}} are detached. 
#' @param unload logical. \code{TRUE} also unloads the packages using \code{\link[=detach]{detach(name, unload = TRUE)}}.
#' @param include.self logical. \code{TRUE} also includes the \code{fastversechild} package - only applicable if \code{\dots} is left empty.  
#' @param force logical. should a \emph{fastversechild} package be detached / unloaded even though other attached packages depend on it?
#' @param session logical. \code{TRUE} also removes the packages from \code{options("fastversechild.extend")}, so they will not be attached again with \code{library(fastversechild)} in the current session. If \code{\dots} is left empty and \code{include.self = TRUE}, this will clear \bold{all} \emph{fastversechild} options set for the session. 
#' 
#' @returns \code{fastversechild_detach} returns \code{NULL} invisibly. 
#' @seealso \code{\link{fastversechild_extend}}, \code{\link{fastversechild}}
#' @export
fastversechild_detach <- function(..., unload = FALSE, force = FALSE, include.self = TRUE, 
                             session = FALSE) {
  
  if(missing(...)) {
    loaded <- ckeck_attached(needed = FALSE)
    if(include.self) loaded <- c(loaded, "fastversechild") # Could already be part of loaded ??
    if(session) {
      options(fastversechild.extend = NULL)
      if(include.self) options(fastversechild.quiet = NULL, 
                               fastversechild.styling = NULL)
    }
  } else {
    pkg <- tryCatch(c(...), error = function(e) .c(...))
    if(!is.character(pkg) || length(pkg) > 200L) pkg <- .c(...)
    loaded <- pkg[is_attached(pkg)] # Include self? -> nope, not sensible...
    if(session) {
      epkg <- getOption("fastversechild.extend")
      if(length(epkg)) options(fastversechild.extend = if(length(pdiff <- setdiff(epkg, pkg))) pdiff else NULL)
    }
  }
  
  if(length(loaded)) {
    ul <- paste0("package:", loaded) 
    for(i in ul)  eval(substitute(detach(pkgi, unload = unload, 
                                         character.only = TRUE, force = force), list(pkgi = i)))
  }
  
  invisible()
}


#' Extend the fastversechild
#' 
#' Loads additional packages as part of the \emph{fastversechild}. 
#' 
#' @param \dots comma-separated package names, quoted or unquoted, or vectors of package names. 
#' @param install logical. Install packages not available?
#' @param check.conflicts logical. Should conflicts between extension packages and attached packages be checked?
#' 
#' @details 
#' 
#' When the \emph{fastversechild} is extended calling \code{fastversechild_extend(...)}, the packages that are not attached are attached, but conflicts are checked for all specified packages. 
#' An \code{options("fastversechild.extend")} is set which stores these extension packages, regardless of whether they were already attached or not. When calling 
#' \code{\link{fastversechild_packages}}, \code{\link{fastversechild_deps}}, \code{\link{fastversechild_conflicts}}, \code{\link{fastversechild_update}}, \code{\link{fastversechild_sitrep}} or \code{\link{fastversechild_detach}}, these packages are included as part of the \emph{fastversechild}. 
#' To extend the \emph{fastversechild} for the current session when it is not yet loaded, users can also set \code{options(fastversechild.extend = c(...))}, where \code{c(...)}
#' is a character vector of package names, before calling \code{library(fastversechild)}. 
#' 
#' @returns \code{fastversechild_extend} returns \code{NULL} invisibly. 
#' @seealso \code{\link{fastversechild_detach}}, \code{\link{fastversechild}}
#' @export
fastversechild_extend <- function(..., install = FALSE, 
                             check.conflicts = !isTRUE(getOption("fastversechild.quiet"))) {
  
  if(!missing(...)) {
    epkg <- tryCatch(c(...), error = function(e) .c(...))
    if(!is.character(epkg) || length(epkg) > 200L) epkg <- .c(...)
  } else stop("Need to supply package names to ...")
  
  pkg <- fastversechild_packages(extensions = TRUE, include.self = FALSE)
  
  if(length(ex <- getOption("fastversechild.extend"))) { # It was already extended before
    options(fastversechild.extend = unique(c(ex, setdiff(epkg, pkg))))
  } else { # It is extended for the first time
    options(fastversechild.extend = setdiff(epkg, pkg))
  }
  
  needed <- epkg[!is_attached(epkg)]
  if(length(needed) == 0L) return(invisible())

  if(install) {
    inst <- needed[!is_installed(needed)]
    if(length(inst)) {
      install.packages(inst)
      cat("\n")
    }
  } 
  
  fastversechild_attach(needed, "Attaching extension packages")

  if(check.conflicts && !"package:conflicted" %in% search()) {
    x <- fastversechild_conflicts(epkg)
    if(length(x)) msg(fastversechild_conflict_message(x))
  }
  
  invisible()
}


