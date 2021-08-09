
msg <- function(..., startup = FALSE) {
  if(!isTRUE(getOption("fastverse.quiet"))) {
      if(startup) packageStartupMessage(...) else message(...)
  }
}

gconf_path <- function() paste(system.file(package = 'fastverse'), '.fastverse', sep = '/')

project_packages <- function() {
  fileConn <- file(".fastverse")
  pkg <- readLines(fileConn, warn = FALSE, skipNul = TRUE)
  close(fileConn)
  pkg <- trimws(unlist(strsplit(pkg, ", | ,|,| "), use.names = FALSE)) # This will always work!
  pkg <- pkg[nzchar(pkg)]
  if(!length(pkg)) stop("Empty config file. Please write package names into your .fastverse config file, separated by commas, spaces or line breaks.")
  pkg
}

#' List all packages in the fastverse
#' 
#' Core packages are first fetched from a project-level configuration file (if found), else from a global configuration file (if found), 
#' otherwise the standard set of core packages is returned. In addition, if \code{extensions = TRUE}, any packages used to extend the \emph{fastverse} for the current 
#' session are also returned. 
#' 
#' @param extensions logical. \code{TRUE} appends the set of core packages with all packages found in \code{options("fastverse.extend")}. 
#' @param include.self logical. Include the \emph{fastverse} package in the list?
#' 
#' @returns A character vector of package names. 
#' @export
#' @seealso \code{\link{fastverse_extend}}, \code{\link{fastverse}}
#' @examples
#' fastverse_packages()
fastverse_packages <- function(extensions = TRUE, include.self = TRUE) {
  if(file.exists(".fastverse")) {
    pkg <- project_packages()
  } else {
    ext_pkg_file <- gconf_path()
    if(file.exists(ext_pkg_file)) {
      fileConn <- file(ext_pkg_file)
      pkg <- readLines(fileConn)
      close(fileConn)
    } else pkg <- .core_pkg
  }
  if(extensions && length(ex <- getOption("fastverse.extend"))) pkg <- unique(c(pkg, ex))
  if(include.self) pkg <- c(pkg, "fastverse")
  pkg
}

#' Reset the fastverse to defaults
#' 
#' Calling this function will remove global configuration files and (default) clear all package options. 
#' Attached packages will not be detached, and configuration files for projects (as discussed in the vignette) will not be removed. 
#'
#' @param options logical. \code{TRUE} also clears all \emph{fastverse} options. 
#' @seealso \code{\link{fastverse_extend}}, \code{\link{fastverse}}
#' @export
fastverse_reset <- function(options = TRUE) {
  ext_file <- gconf_path()
  if(file.exists(ext_file)) file.remove(ext_file)
  if(options) options(fastverse.extend = NULL, 
                      fastverse.quiet = NULL, 
                      fastverse.styling = NULL)
  invisible()
}

# Not needed anymore
# invert <- function(x) {
#   if (length(x) == 0) return()
#   stacked <- unclass(utils::stack(x))
#   ### tapply(as.character(stacked$ind), stacked$values, list) # Old
#   split(as.character(stacked$ind), stacked$values) # Faster
# }

package_version <- function(x) paste(unclass(packageVersion(x))[[1L]], collapse = ".")

green <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[32m", x, "\033[39m")
blue <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[34m", x, "\033[39m")
cyan <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[36m", x, "\033[39m")
magenta <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[35m", x, "\033[39m")
magenta2 <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[38;5;198m", x, "\033[39m")  #ff0066
gold <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[38;5;214m", x, "\033[39m") #fa9c19
lightblue <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[38;5;45m", x, "\033[39m")                                    # 12b8ff
kingsblue <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[38;5;33m", x, "\033[39m")                                      #0062ff
# grey09 <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[38;5;253m", x, "\033[39m") 
grey70 <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[0;38;5;249m", x, "\033[39m") 
red <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[31m", x, "\033[39m")
yellow <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[33m", x, "\033[39m")
bold <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[1m", x, "\033[22m")
# Crayons white is more gray-isch
# white <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[37m", x, "\033[39m")
# Using bright white: https://i.stack.imgur.com/9UVnC.png
# white <- function(x) if(isFALSE(getOption("fastverse.styling"))) x else paste0("\033[97m", x, "\033[39m")
# Problem: If console is white: cannot read..
text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!identical(.Platform$GUI, "RStudio")) return(x)
  grey70(x)
}

rule <- function(left, right = NULL, style.left = identity, style.right = identity, style.rule = FALSE) {
  n <- .Options$width
  left <- as.character(left)
  if(length(right)) {
    right <- as.character(right)
    w <- n - nchar(left) - nchar(right) - 8L
    if(style.rule) {
      res <- paste(c(text_col("-- "), style.left(left), " ", text_col(paste(rep("-", w), collapse = "")), " ", style.right(right), text_col(" --")), collapse = "")
    } else {
      res <- paste(c("-- ", style.left(left), " ", rep("-", w), " ", style.right(right), " --"), collapse = "")
    }
  } else {
    w <- n - nchar(left) - 4L
    if(style.rule) {
      res <- paste(c(text_col("-- "), style.left(left), " ", text_col(paste(rep("-", w), collapse = ""))), collapse = "")
    } else {
      res <- paste(c("-- ", style.left(left), " ", rep("-", w)), collapse = "") 
    }
  }
  class(res) <- "fvrule"
  res
}

# Not needed, but better than not.. 
#' @export
print.fvrule <- function(x, ..., sep = "\n") {
  cat(x, ..., sep = sep)
  invisible(x)
}
