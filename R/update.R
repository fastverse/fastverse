#' Update fastverse packages
#'
#' This will check to see if all fastverse packages (and optionally, their
#' dependencies) are up-to-date, and will install after an interactive
#' confirmation.
#'
#' @inheritParams fastverse_deps
#' @export
#' @examples
#' \dontrun{
#' fastverse_update()
#' }
fastverse_update <- function(recursive = FALSE, repos = getOption("repos")) {
  
  deps <- fastverse_deps(recursive, repos)
  behind <- subset(deps, behind)
  
  if (nrow(behind) == 0) {
    cat("All fastverse packages up-to-date\n")
    return(invisible())
  }
  
  cat("The following packages are out of date:\n")
  cat("\n", paste0("* ", magenta2(format(behind$package)), " (", behind$local, " -> ", behind$cran, ")\n"))
  
  cat("\nStart a clean R session then run:\n")
  
  pkg_str <- paste0(deparse(behind$package), collapse = "\n")
  cat("install.packages(", pkg_str, ")\n", sep = "")
  
  invisible()
}

#' Get a situation report on the fastverse
#'
#' This function gives a quick overview of the versions of R and RStudio as
#' well as all fastverse packages. It's primarily designed to help you get
#' a quick idea of what's going on when you're helping someone else debug
#' a problem.
#'
#' @export
fastverse_sitrep <- function() {
  #kingsblue()
  rule(paste0("fastverse ", package_version("fastverse"), ": Core packages"), 
       paste("R", getRversion()))

  deps <- fastverse_deps()
  package_pad <- format(deps$package)
  packages <- ifelse(
    deps$behind,
    paste0("* ", gold(bold(package_pad)), " (", deps$local, " < ", deps$cran, ")\n"),
    paste0("* ", magenta2(package_pad), " (", deps$cran, ")\n")
  )
  
  cat("\n", packages[deps$package %in% .core_pck])
  rule("Non-core packages")
  cat("\n", packages[!deps$package %in% .core_pck])
}

packageVersion2 <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) utils::packageVersion(pkg) else 0
}

#' List all \emph{fastverse} dependencies
#'
#' @param recursive if \code{TRUE}, will also list all dependencies of
#'   \emph{fastverse} packages.
#' @param repos the repositories to use to check for updates.
#'   Defaults to \code{getOptions("repos")}.
#' @export
fastverse_deps <- function(recursive = FALSE, repos = getOption("repos")) {
  pkgs <- utils::available.packages(repos = repos)
  pck <- fastverse_packages(include.self = FALSE)
  # if(identical(pck, .core_pck)) pck <- "fastverse"
  deps <- tools::package_dependencies(pck, pkgs, recursive = recursive)
  
  pkg_deps <- unique(sort(unlist(deps)))
  
  base_pkgs <- c(
    "base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tools", "tcltk",
    "utils"
  )
  pkg_deps <- setdiff(pkg_deps, base_pkgs)
  
  # tool_pkgs <- c("cli", "crayon", "rstudioapi")
  # pkg_deps <- setdiff(pkg_deps, tool_pkgs)
  
  cran_version <- lapply(pkgs[pkg_deps, "Version"], base::package_version)
  local_version <- lapply(pkg_deps, packageVersion2)
  
  behind <- mapply(`>`, cran_version, local_version)
  
  data.frame(
    package = pkg_deps,
    cran = sapply(cran_version, as.character),
    local = sapply(local_version, as.character),
    behind = behind
  )
}

