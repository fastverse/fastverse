packageVersion2 <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) packageVersion(pkg) else 0
}

#' List all fastversechild dependencies
#' 
#' Lists all \emph{fastversechild} dependencies and the local and CRAN versions of packages and dependencies.
#'
#' @param pkg character vector of packages to check dependencies and versions of. The default is all \emph{fastversechild} packages. 
#' @param recursive logical. \code{TRUE} recursively determines all packages required to operate these packages.
#' \code{FALSE} will only list the packages and their direct dependencies. 
#' @param repos the repositories to use to check for updates. Defaults to \code{getOptions("repos")}.
#' @param include.self logical. \code{TRUE} also includes the \emph{fastversechild} package and checks against the CRAN version.  
#' @param check.deps logical. \code{FALSE} will not determine dependencies but only display the update status of packages in \code{pkg}. 
#' 
#' @returns A data frame giving the package names, the CRAN and local version, and a logical variable stating whether the local version is behind the CRAN version. 
#' @seealso \code{\link{fastversechild_sitrep}}, \code{\link{fastversechild}}
#' @export
fastversechild_deps <- function(pkg = fastversechild_packages(), recursive = FALSE, 
                           repos = getOption("repos"), include.self = FALSE, check.deps = TRUE) {  
  
  pkgs <- available.packages(repos = repos)
  if(!length(pkgs)) stop("Please connect to the internet to execute this function")
  fv <- "fastversechild"
  pkg <- pkg[pkg != fv] # Code should work regardless of whether pkg includes "fastversechild" or not
  if(check.deps) {
    if(!include.self) fv <- NULL
    deps <- package_dependencies(pkg, pkgs, recursive = recursive)
    pkg_deps <- unique(c(pkg, fv, sort(unlist(deps, use.names = FALSE)))) 
    base_pkgs <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
      "methods", "parallel", "splines", "stats", "stats4", "tools", "tcltk", "utils")
    pkg_deps <- setdiff(pkg_deps, base_pkgs)
  } else {
    pkg_deps <- if(include.self) c(pkg, fv) else pkg
  }
  
  cran_version <- lapply(pkgs[pkg_deps, "Version"], base::package_version)
  local_version <- lapply(pkg_deps, packageVersion2)
  
  behind <- mapply(`>`, cran_version, local_version)
  
  data.frame(
    package = pkg_deps,
    cran = sapply(cran_version, as.character),
    local = sapply(local_version, as.character),
    behind = behind,
    row.names = seq_along(pkg_deps)
  )
}


#' Update fastversechild packages
#'
#' This will check all \emph{fastversechild} packages (and their
#' dependencies) for updates and (optionally) install those updates. 
#'
#' @param \dots arguments passed to \code{\link{fastversechild_deps}}.
#' @param install logical. \code{TRUE} will proceed to install outdated packages, whereas \code{FALSE} (recommended) will print the installation command asking you to run it in a clean R session.
#' 
#' @returns \code{fastversechild_update} returns \code{NULL} invisibly. 
#' @seealso \code{\link{fastversechild_deps}}, \code{\link{fastversechild}}
#' @export
fastversechild_update <- function(..., install = FALSE) {
  
  deps <- fastversechild_deps(...) 
  behind <- subset(deps, behind)
  
  if (nrow(behind) == 0L) {
    if(!isTRUE(getOption("fastversechild.quiet"))) cat("All fastversechild packages up-to-date\n")
    return(invisible())
  }
  
  if(!isTRUE(getOption("fastversechild.quiet"))) {
    cat("The following packages are out of date:\n")
    cat("\n", paste0("* ", gold(format(behind$package)), " (", behind$local, " -> ", behind$cran, ")\n"))
  }
  
  if(install) {
    install.packages(behind$package)
  } else {
    cat("\nStart a clean R session then run:\n")
    pkg_str <- paste0(deparse(behind$package), collapse = "\n")
    cat("install.packages(", pkg_str, ")\n", sep = "")
  }
  
  invisible()
}

#' Install (missing) fastversechild packages
#'
#' This function (by default) checks if any \emph{fastversechild} package is missing and installs the missing package(s).
#'
#' @param \dots comma-separated package names, quoted or unquoted, or vectors of package names. If left empty, all packages returned by \code{\link{fastversechild_packages}} are checked. 
#' @param only.missing logical. \code{TRUE} only installs packages that are unavailable. \code{FALSE} installs all packages, even if they are available. 
#' @param install logical. \code{TRUE} will proceed to install packages, whereas \code{FALSE} (recommended) will print the installation command asking you to run it in a clean R session.
#' 
#' @note 
#' There is also the possibility to set \code{options(fastversechild.install = TRUE)} before \code{library(fastversechild)}, which will call \code{fastversechild_install()} before loading any packages to make sure all packages are available.
#' If you are using a \code{.fastversechild} configuration file inside a project (see vignette), you can also place \code{_opt_fastversechild.install = TRUE} before the list of packages in that file.
#' 
#' 
#' @returns \code{fastversechild_install} returns \code{NULL} invisibly. 
#' @seealso \code{\link{fastversechild_update}}, \code{\link{fastversechild}}
#' @export
fastversechild_install <- function(..., only.missing = TRUE, install = TRUE) {
  
  if(missing(...)) {
    pkg <- fastversechild_packages(include.self = FALSE)
  } else {
    pkg <- tryCatch(c(...), error = function(e) .c(...))
    if(!is.character(pkg) || length(pkg) > 200L) pkg <- .c(...)
  }
  
  needed <- if(only.missing) pkg[!is_installed(pkg)] else pkg
  
  if(length(needed)) {
    if(install) {
      install.packages(needed)
    } else {
      cat("\nStart a clean R session then run:\n")
      pkg_str <- paste0(deparse(needed), collapse = "\n")
      cat("install.packages(", pkg_str, ")\n", sep = "")
    }
  } else if(!isTRUE(getOption("fastversechild.quiet"))) {
    cat("All fastversechild packages installed\n")
  }
  
  return(invisible())
}


#' Get a situation report on the fastversechild
#'
#' This function gives a quick overview of the version of R and all 
#' \emph{fastversechild} packages (including availability updates for packages) and indicates 
#' whether any project-level configuration files are used.
#' 
#' @param \dots arguments other than \code{pkg} passed to \code{\link{fastversechild_deps}}.
#' 
#' @returns \code{fastversechild_sitrep} returns \code{NULL} invisibly. 
#' @seealso \code{\link{fastversechild_deps}}, \code{\link{fastversechild}}
#' @export
fastversechild_sitrep <- function(...) {

  cat(rule(paste0("fastversechild ", package_version("fastversechild"), ": Situation Report"), 
       paste("R", getRversion()), 
       style.left = function(x) sub("Situation Report", bold("Situation Report"), 
                                    sub("fastversechild", kingsblue("fastversechild"), x, fixed = TRUE), fixed = TRUE)))

  pkg <- fastversechild_packages(include.self = FALSE) 
  deps <- fastversechild_deps(pkg, ...)  

  package_pad <- format(deps$package)
  packages <- ifelse(
    deps$behind,
    paste0("* ", gold(package_pad), " (", deps$local, " < ", deps$cran, ")\n"), 
    paste0("* ", magenta2(package_pad), " (", deps$cran, ")\n")
  )
  
  deps <- deps$package
  
  ex <- getOption("fastversechild.extend")
  if(length(ex)) pkg <- setdiff(pkg, ex)
  if(any(deps == "fastversechild")) pkg <- c(pkg, "fastversechild") # include.self = FALSE above makes sure we don't add it twice here
  
  pcol <- file.exists(".fastversechild")
  cat("\n", paste0("* Project config file: ", pcol, "\n"))
  cat(rule("Core packages"), "\n", packages[deps %in% pkg])
  if(length(ex)) {
    cat(rule("Extension packages"), "\n", packages[deps %in% ex])
    pkg <- c(pkg, ex)
  }
  if(missing(...) || !any(cdl <- ...names() == "check.deps") || ...elt(which(cdl))) 
    cat(rule("Dependencies"), "\n", packages[!deps %in% pkg])
  
  invisible()
}


