
msg <- function(..., startup = FALSE) {
  if(!isTRUE(getOption("fastverse_quiet"))) {
      if(startup) packageStartupMessage(...) else message(...)
  }
}

gconf_path <- function() paste(system.file(package = 'fastverse'), '.fastverse', sep = '/')

project_packages <- function() {
  fileConn <- file(".fastverse")
  pck <- readLines(fileConn, warn = FALSE, skipNul = TRUE)
  close(fileConn)
  pck <- unlist(strsplit(pck, c(" ", ",")), use.names = FALSE)
  pck[nzchar(pck)]
}

#' List all packages in the fastverse
#'
#' @param extended logical. \code{TRUE} return all packages currently loaded either with \code{library(fastverse)} or \code{\link{fastverse_extend}}. 
#' \code{FALSE} only returns the core fastverse packages and any packages added using \code{\link[=fastverse_extend]{fastverse_extend(..., permanent = TRUE)}}.
#' @param include.self logical. Include the \emph{fastverse} package in the list?
#' @export
#' @seealso \code{\link{fastverse}}
#' @examples
#' fastverse_packages()
fastverse_packages <- function(extended = TRUE, include.self = TRUE) {
  if(file.exists(".fastverse")) {
    pck <- project_packages()
  } else {
    ext_pck_file <- gconf_path()
    if(file.exists(ext_pck_file)) {
      fileConn <- file(ext_pck_file)
      pck <- readLines(fileConn)
      close(fileConn)
    } else pck <- .core_pck
  }
  if(extended && length(ex <- getOption("fastverse_extend"))) pck <- unique(c(pck, ex))
  if(include.self) pck <- c(pck, "fastverse")
  pck
}

#' Reset the fastverse to defaults
#' 
#' Calling this function will remove any permanent package extensions and (default) clear all package options. 
#' Packages loaded will not be unloaded, and configuration files for projects (as discussed in the vignette) will not be removed. 
#'
#' @param options logical. \code{TRUE} also clears all \emph{fastverse} options. 
#' @seealso \code{\link{fastverse_extend}}, \code{\link{fastverse}}
#' @export
fastverse_reset <- function(options = TRUE) {
  if(options) options(fastverse_extend = NULL, 
                      fastverse_quiet = NULL, 
                      fastverse_styling = NULL)
  ext_file <- gconf_path()
  if(file.exists(ext_file)) file.remove(ext_file)
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

green <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[32m", x, "\033[39m")
blue <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[34m", x, "\033[39m")
cyan <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[36m", x, "\033[39m")
magenta <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[35m", x, "\033[39m")
magenta2 <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[38;5;198m", x, "\033[39m")  #ff0066
gold <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[38;5;214m", x, "\033[39m") #fa9c19
lightblue <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[38;5;45m", x, "\033[39m")                                    # 12b8ff
kingsblue <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[38;5;33m", x, "\033[39m")                                      #0062ff
# grey09 <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[38;5;253m", x, "\033[39m") 
grey70 <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[0;38;5;249m", x, "\033[39m") 
red <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[31m", x, "\033[39m")
yellow <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[33m", x, "\033[39m")
bold <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[1m", x, "\033[22m")
# Crayons white is more gray-isch
# white <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[37m", x, "\033[39m")
# Using bright white: https://i.stack.imgur.com/9UVnC.png
# white <- function(x) if(isFALSE(getOption("fastverse_styling"))) x else paste0("\033[97m", x, "\033[39m")
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
print.fvrule <- function(x, ..., sep = "\n") {
  cat(x, ..., sep = sep)
  invisible(x)
}
