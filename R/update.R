packageVersion2 <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) packageVersion(pkg) else 0
}

#' List all fastverse dependencies
#' 
#' Lists all \emph{fastverse} dependencies and the local and repository (e.g. CRAN) versions of packages and dependencies.
#' 
#' @inheritParams fastverse_update
#' @param pkg character vector of packages to check dependencies and versions of. The default is all \emph{fastverse} packages. 
#' @param recursive logical. \code{TRUE} recursively determines all packages required to operate these packages.
#' \code{FALSE} will only list the packages and their direct dependencies. 
#' @param include.self logical. \code{TRUE} also includes the \emph{fastverse} package and checks against the repository version.  
#' @param check.deps logical. \code{FALSE} will not determine dependencies but only display the update status of packages in \code{pkg}. 
#' 
#' @returns A data frame giving the package names, the repository and local version, and a logical variable stating whether the local version is behind the repository version. 
#' @seealso \code{\link{fastverse_sitrep}}, \code{\link{fastverse}}
#' @export
fastverse_deps <- function(pkg = fastverse_packages(), recursive = FALSE, 
                           repos = getOption("repos"), include.self = FALSE, check.deps = TRUE) {  
  
  pkgs <- available.packages(repos = repos)
  if(!length(pkgs)) stop("Please connect to the internet to execute this function")
  fv <- "fastverse"
  pkg <- pkg[pkg != fv] # Code should work regardless of whether pkg includes "fastverse" or not!!
  if(check.deps) {
    if(!include.self) fv <- NULL
    if(nrow(pkgs) < 10000L) { # if repository is not CRAN, add CRAN packages to check deps
      cran_pkgs <- available.packages(repos = "https://cloud.r-project.org")
      pkgs <- rbind(pkgs, cran_pkgs[!rownames(cran_pkgs) %in% rownames(pkgs), ])
    }
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
    repos = sapply(cran_version, as.character),
    local = sapply(local_version, as.character),
    behind = behind,
    row.names = seq_along(pkg_deps)
  )
}


repos_str <- function(x) {
  if(length(x) == 1L) x else if(length(names(x)))
  paste0("c(", paste(paste0(names(x), ' = "', x), collapse = '", '), '")') else 
  paste0('c("', paste(x, collapse = '", "'), '")')
}

#' Update fastverse packages
#' @aliases .fastverse_repos
#' @description 
#' This will check all \emph{fastverse} packages (and their
#' dependencies) for updates and (optionally) install those updates. 
#'
#' @param \dots arguments passed to \code{\link{fastverse_deps}}.
#' @param install logical. \code{TRUE} will proceed to install outdated packages, whereas \code{FALSE} (recommended) will print the installation command asking you to run it in a clean R session.
#' @param repos character vector. Base URL(s) of the repositories to use, e.g., the URL of a CRAN mirror such as \code{"https://cloud.r-project.org"}. 
#' The macro \code{.fastverse_repos} contains the URL of the \href{https://fastverse.r-universe.dev}{fastverse r-universe server} to check/install the development version of packages.
#' 
#' @returns \code{fastverse_update} returns \code{NULL} invisibly. 
#' @seealso \code{\link{fastverse_deps}}, \code{\link{fastverse}}
#' 
#' @examples \dontrun{
#' ## Update from CRAN
#' fastverse_update()
#' 
#' ## Update from R-Universe (development versions)
#' fastverse_update(repos = .fastverse_repos)
#' }
#' @export
fastverse_update <- function(..., install = FALSE, repos = getOption("repos")) {
  
  deps <- fastverse_deps(..., repos = repos) 
  behind <- subset(deps, behind)
  
  if (nrow(behind) == 0L) {
    if(!isTRUE(getOption("fastverse.quiet"))) cat("All fastverse packages up-to-date\n")
    return(invisible())
  }
  
  if(!isTRUE(getOption("fastverse.quiet"))) {
    cat("The following packages are out of date:\n")
    cat("\n", paste0("* ", gold(format(behind$package)), " (", behind$local, " -> ", behind$repos, ")\n"))
  }
  
  if(install) {
    install.packages(behind$package, repos = repos)
  } else {
    cat("\nStart a clean R session then run:\n")
    pkg_str <- paste0(deparse(behind$package), collapse = "\n")
    if(identical(repos, getOption("repos")))
      cat("install.packages(", pkg_str, ")\n", sep = "")
    else 
      cat("install.packages(", pkg_str, ", repos = ", repos_str(repos), ")\n", sep = "")
  }
  
  invisible()
}

#' @export
.fastverse_repos <- c(fastverse = 'https://fastverse.r-universe.dev', CRAN = 'https://cloud.r-project.org')

#' Install (missing) fastverse packages
#'
#' This function (by default) checks if any \emph{fastverse} package is missing and installs the missing package(s). The development versions of \emph{fastverse} packages can also be installed from \href{"https://fastverse.r-universe.dev"}{r-universe}. The link to the repository is contained in the \code{.fastverse_repos} macro. 
#' 
#' @inheritParams fastverse_update
#' @param \dots comma-separated package names, quoted or unquoted, or vectors of package names. If left empty, all packages returned by \code{\link{fastverse_packages}} are checked. 
#' @param only.missing logical. \code{TRUE} only installs packages that are unavailable. \code{FALSE} installs all packages, even if they are available. 
#' @param install logical. \code{TRUE} will proceed to install packages, whereas \code{FALSE} (recommended) will print the installation command asking you to run it in a clean R session.
#' 
#' @note 
#' There is also the possibility to set \code{options(fastverse.install = TRUE)} before \code{library(fastverse)}, which will call \code{fastverse_install()} before loading any packages to make sure all packages are available.
#' If you are using a \code{.fastverse} configuration file inside a project (see vignette), you can also place \code{_opt_fastverse.install = TRUE} before the list of packages in that file.
#' 
#' 
#' @returns \code{fastverse_install} returns \code{NULL} invisibly. 
#' @seealso \code{\link{fastverse_update}}, \code{\link{fastverse}}
#' @export
fastverse_install <- function(..., only.missing = TRUE, install = TRUE, repos = getOption("repos")) {
  
  if(missing(...)) {
    pkg <- fastverse_packages(include.self = FALSE)
  } else {
    pkg <- tryCatch(c(...), error = function(e) c_(...))
    if(!is.character(pkg) || length(pkg) > 200L) pkg <- c_(...)
  }
  
  needed <- if(only.missing) pkg[!is_installed(pkg)] else pkg
  
  if(length(needed)) {
    if(install) {
      install.packages(needed, repos = repos)
    } else {
      cat("\nStart a clean R session then run:\n")
      pkg_str <- paste0(deparse(needed), collapse = "\n")
      if(identical(repos, getOption("repos")))
        cat("install.packages(", pkg_str, ")\n", sep = "")
      else 
        cat("install.packages(", pkg_str, ", repos = ", repos_str(repos), ")\n", sep = "")
    }
  } else if(!isTRUE(getOption("fastverse.install")) && !isTRUE(getOption("fastverse.quiet"))) {
    cat("All fastverse packages installed\n")
  }
  
  return(invisible())
}


#' Get a situation report on the fastverse
#'
#' This function gives a quick overview of the version of R and all 
#' \emph{fastverse} packages (including availability updates for packages) and indicates 
#' whether any global or project-level configuration files are used
#' (as described in more detail the vignette). 
#' 
#' @param \dots arguments other than \code{pkg} passed to \code{\link{fastverse_deps}}.
#' 
#' @returns \code{fastverse_sitrep} returns \code{NULL} invisibly. 
#' @seealso \code{\link{fastverse_deps}}, \code{\link{fastverse}}
#' @export
fastverse_sitrep <- function(...) {

  cat(rule(paste0("fastverse ", package_version("fastverse"), ": Situation Report"), # , ": Core packages" 
       paste("R", getRversion()), 
       style.left = function(x) sub("Situation Report", bold("Situation Report"), 
                                    sub("fastverse", kingsblue("fastverse"), x, fixed = TRUE), fixed = TRUE)))

  pkg <- fastverse_packages(include.self = FALSE) # need include.self = FALSE here !!
  deps <- fastverse_deps(pkg, ...)  

  package_pad <- format(deps$package)
  packages <- ifelse(
    deps$behind,
    paste0("* ", gold(package_pad), " (", deps$local, " < ", deps$repos, ")\n"), # bold()
    paste0("* ", magenta2(package_pad), " (", deps$local, ")\n")
  )
  
  deps <- deps$package
  
  ex <- getOption("fastverse.extend")
  if(length(ex)) pkg <- setdiff(pkg, ex)
  if(any(deps == "fastverse")) pkg <- c(pkg, "fastverse") # include.self = FALSE above makes sure we don't add it twice here
  
  glcol <- file.exists(gconf_path())
  pcol <- file.exists(".fastverse")
  cat("\n", c(paste0("* Global config file: ", glcol, if(glcol && pcol) " (ignored)\n" else "\n"),
              paste0("* Project config file: ", pcol, "\n")))
  cat(rule("Core packages"), "\n", packages[deps %in% pkg])
  if(length(ex)) {
    cat(rule("Extension packages"), "\n", packages[deps %in% ex])
    pkg <- c(pkg, ex)
  }
  if(missing(...) || !any(names(dots <- list(...)) == "check.deps") || dots[["check.deps"]]) 
    cat(rule("Dependencies"), "\n", packages[!deps %in% pkg])
  
  invisible()
}


