packageVersion2 <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) utils::packageVersion(pkg) else 0
}

deps_core <- function(pck, recursive = FALSE, repos = getOption("repos"), include.self = FALSE, check.deps = TRUE) {  
  
  pkgs <- utils::available.packages(repos = repos)
  fv <- "fastverse"
  pck <- pck[pck != fv] # Code should work regardless of whether pck includes "fastverse" or not!!s
  if(check.deps) {
    if(!include.self) fv <- NULL
    deps <- tools::package_dependencies(pck, pkgs, recursive = recursive)
    pkg_deps <- unique(c(pck, fv, sort(unlist(deps, use.names = FALSE)))) 
    base_pkgs <- c(
      "base", "compiler", "datasets", "graphics", "grDevices", "grid",
      "methods", "parallel", "splines", "stats", "stats4", "tools", "tcltk", "utils"
    )
    pkg_deps <- setdiff(pkg_deps, base_pkgs)
    # tool_pkgs <- c("cli", "crayon", "rstudioapi")
    # pkg_deps <- setdiff(pkg_deps, tool_pkgs)
  } else {
    pkg_deps <- if(include.self) c(pck, fv) else pck
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

#' List all fastverse dependencies
#' 
#' Lists all fastverse dependencies.
#'
#' @param recursive logical. \code{TRUE} recursively determines all packages required to operate the current set of \emph{fastverse} packages.
#' \code{FALSE} will only list the \emph{fastverse} packages and direct dependencies of those packages. 
#' @param repos the repositories to use to check for updates. Defaults to \code{getOptions("repos")}.
#' @param include.self logical. \code{TRUE} also includes the \emph{fastverse} package and checks against CRAN updates.  
#' 
#' @returns A data frame giving the package names, the CRAN and local version, and a logical variable stating whether the local version is behind the CRAN version. 
#' @seealso \code{\link{fastverse_sitrep}}, \code{\link{fastverse}}
#' @export
fastverse_deps <- function(recursive = FALSE, repos = getOption("repos"), include.self = FALSE) {
  pck <- fastverse_packages()
  deps_core(pck, recursive, repos, include.self)
}


#' Update fastverse packages
#'
#' This will check all fastverse packages (and their
#' dependencies) for updates and print an install command. 
#'
#' @param ckeck.deps logical. \code{TRUE} also checks for updates in dependencies of \emph{fastverse} packages. 
#' @param \dots arguments passed to \code{\link{fastverse_deps}}.
#' 
#' @seealso \code{\link{fastverse_deps}}, \code{\link{fastverse}}
#' @export
fastverse_update <- function(check.deps = TRUE, ...) {
  
  if(check.deps) {
    deps <- fastverse_deps(...) 
  } else {
    deps <- deps_core(fastverse_packages(), ..., check.deps = FALSE)
  }
  
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
#' whether any global or project-level configuration files are in use
#' (as described in the vignette). 
#' 
#' @param ckeck.deps logical. \code{TRUE} also checks for updates in dependencies of \emph{fastverse} packages. 
#' @param \dots arguments passed to \code{\link{fastverse_deps}}.
#' 
#' @seealso \code{\link{fastverse_deps}}, \code{\link{fastverse}}
#' @export
fastverse_sitrep <- function(check.deps = TRUE, ...) {

  cat(rule(paste0("fastverse ", package_version("fastverse"), ": Situation Report"), # , ": Core packages" 
       paste("R", getRversion()), 
       style.left = function(x) sub("Situation Report", bold("Situation Report"), 
                                    sub("fastverse", kingsblue("fastverse"), x, fixed = TRUE), fixed = TRUE)))

  pck <- fastverse_packages(include.self = FALSE) # need include.self = FALSE here !!
  deps <- deps_core(pck, ..., check.deps = check.deps)  

  package_pad <- format(deps$package)
  packages <- ifelse(
    deps$behind,
    paste0("* ", gold(package_pad), " (", deps$local, " < ", deps$cran, ")\n"), # bold()
    paste0("* ", magenta2(package_pad), " (", deps$cran, ")\n")
  )
  
  deps <- deps$package
  
  ex <- getOption("fastverse_extend")
  if(length(ex)) pck <- setdiff(pck, ex)
  if(any(deps == "fastverse")) pck <- c(pck, "fastverse") # include.self = FALSE above makes sure we don't add it twice here
  
  glcol <- file.exists(gconf_path())
  pcol <- file.exists(".fastverse")
  cat("\n", c(paste0("* Global config file: ", glcol, if(glcol && pcol) " (ignored)\n" else "\n"),
              paste0("* Project config file: ", pcol, "\n")))
  cat(rule("Core packages"), "\n", packages[deps %in% pck])
  if(length(ex)) {
    cat(rule("Extension packages"), "\n", packages[deps %in% ex])
    pck <- c(pck, ex)
  }
  if(check.deps) cat(rule("Dependencies"), "\n", packages[!deps %in% pck])
}


