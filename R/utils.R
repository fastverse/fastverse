
.c <- function (...) as.character(substitute(c(...))[-1L])

is_attached <- function(x) paste0("package:", x) %in% search()
is_installed <- function(x) vapply(x, requireNamespace, TRUE, quietly = TRUE)


msg <- function(..., startup = FALSE) {
  if(!isTRUE(getOption("fastversechild.quiet"))) {
      if(startup) packageStartupMessage(...) else message(...)
  }
}

project_packages <- function() {
  fileConn <- file(".fastversechild")
  pkg <- readLines(fileConn, warn = FALSE, skipNul = TRUE)
  close(fileConn)
  pkg <- trimws(unlist(strsplit(pkg, ", | ,|,| "), use.names = FALSE)) 
  pkg <- pkg[nzchar(pkg)]
  if(!length(pkg)) stop("Empty config file. Please write package names into your .fastversechild config file, separated by commas, spaces or line breaks.")
  pkg
}

#' List all packages in the fastversechild
#' 
#' Core packages are first fetched from a project-level configuration file (if found), otherwise the standard set of core packages is returned. 
#' In addition, if \code{extensions = TRUE}, any packages used to extend the \emph{fastversechild} for the current 
#' session are also returned. 
#' 
#' @param extensions logical. \code{TRUE} appends the set of core packages with all packages found in \code{options("fastversechild.extend")}. 
#' @param include.self logical. Include the \emph{fastversechild} package in the list?
#' 
#' @returns A character vector of package names. 
#' @export
#' @seealso \code{\link{fastversechild_extend}}, \code{\link{fastversechild}}
#' @examples
#' fastversechild_packages()
fastversechild_packages <- function(extensions = TRUE, include.self = TRUE) {
  if(file.exists(".fastversechild")) {
    pkg <- project_packages()
  } else {
    pkg <- .core_pkg
  }
  if(extensions && length(ex <- getOption("fastversechild.extend"))) pkg <- unique(c(pkg, ex))
  if(include.self) pkg <- c(pkg, "fastversechild")
  pkg
}


package_version <- function(x) paste(unclass(packageVersion(x))[[1L]], collapse = ".")

green <- function(x) if(isFALSE(getOption("fastversechild.styling"))) x else paste0("\033[32m", x, "\033[39m")
blue <- function(x) if(isFALSE(getOption("fastversechild.styling"))) x else paste0("\033[34m", x, "\033[39m")
cyan <- function(x) if(isFALSE(getOption("fastversechild.styling"))) x else paste0("\033[36m", x, "\033[39m")
magenta2 <- function(x) if(isFALSE(getOption("fastversechild.styling"))) x else paste0("\033[38;5;198m", x, "\033[39m") 
gold <- function(x) if(isFALSE(getOption("fastversechild.styling"))) x else paste0("\033[38;5;214m", x, "\033[39m") 
lightblue <- function(x) if(isFALSE(getOption("fastversechild.styling"))) x else paste0("\033[38;5;45m", x, "\033[39m")           
kingsblue <- function(x) if(isFALSE(getOption("fastversechild.styling"))) x else paste0("\033[38;5;33m", x, "\033[39m") 
grey70 <- function(x) if(isFALSE(getOption("fastversechild.styling"))) x else paste0("\033[0;38;5;249m", x, "\033[39m") 
red <- function(x) if(isFALSE(getOption("fastversechild.styling"))) x else paste0("\033[31m", x, "\033[39m")
yellow <- function(x) if(isFALSE(getOption("fastversechild.styling"))) x else paste0("\033[33m", x, "\033[39m")
bold <- function(x) if(isFALSE(getOption("fastversechild.styling"))) x else paste0("\033[1m", x, "\033[22m")


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
  class(res) <- "fastversechild_rule"
  res
}

# Not needed, but better than not.. 
#' @export
print.fastversechild_rule <- function(x, ..., sep = "\n") {
  cat(x, ..., sep = sep)
  invisible(x)
}
