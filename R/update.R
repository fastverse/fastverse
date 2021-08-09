packageVersion2 <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) packageVersion(pkg) else 0
}

#' List all fastverse dependencies
#' 
#' Lists all \emph{fastverse} dependencies and the local and CRAN versions of packages and dependencies.
#'
#' @param pkg character vector of packages to check dependencies and versions of. The default is all \emph{fastverse} packages. 
#' @param recursive logical. \code{TRUE} recursively determines all packages required to operate these packages.
#' \code{FALSE} will only list the packages and their direct dependencies. 
#' @param repos the repositories to use to check for updates. Defaults to \code{getOptions("repos")}.
#' @param include.self logical. \code{TRUE} also includes the \emph{fastverse} package and checks against the CRAN version.  
#' @param check.deps logical. \code{FALSE} will not determine dependencies but only display the update status of packages in \code{pkg}. 
#' 
#' @returns A data frame giving the package names, the CRAN and local version, and a logical variable stating whether the local version is behind the CRAN version. 
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


#' Update fastverse packages
#'
#' This will check all \emph{fastverse} packages (and their
#' dependencies) for updates and print a command to install those updates. 
#'
#' @param \dots arguments passed to \code{\link{fastverse_deps}}.
#' 
#' @returns \code{fastverse_update} returns \code{NULL} invisibly. 
#' @seealso \code{\link{fastverse_deps}}, \code{\link{fastverse}}
#' @export
fastverse_update <- function(...) {
  
  deps <- fastverse_deps(...) 
  behind <- subset(deps, behind)
  
  if (nrow(behind) == 0L) {
    cat("All fastverse packages up-to-date\n")
    return(invisible())
  }
  
  cat("The following packages are out of date:\n")
  cat("\n", paste0("* ", gold(format(behind$package)), " (", behind$local, " -> ", behind$cran, ")\n"))
  
  cat("\nStart a clean R session then run:\n")
  
  pkg_str <- paste0(deparse(behind$package), collapse = "\n")
  cat("install.packages(", pkg_str, ")\n", sep = "")
  
  invisible()
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
    paste0("* ", gold(package_pad), " (", deps$local, " < ", deps$cran, ")\n"), # bold()
    paste0("* ", magenta2(package_pad), " (", deps$cran, ")\n")
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
  if(missing(...) || !any(cdl <- ...names() == "check.deps") || ...elt(which(cdl))) 
    cat(rule("Dependencies"), "\n", packages[!deps %in% pkg])
  
  invisible()
}


