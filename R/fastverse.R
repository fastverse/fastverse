#' The fastverse
#' 
#' @description
#' The \emph{fastverse} is an extensible suite of R packages, developed independently by various people,
#' that jointly contribute to the objectives of:
#' \enumerate{
#' \item Speeding up R through heavy use of compiled code (C, C++, Fortran)
#' \item Enabling more complex statistical and data manipulation operations in R
#' \item Reducing the number of dependencies required for advanced computing in R
#' }
#' Inspired by the \code{tidyverse} package, the \code{fastverse} package is a flexible package loader and manager 
#' that allows users to put together their own 'verses' of packages and load them with \code{library(fastverse)}. 
#' 
#' \code{fastverse} installs 4 core packages (\code{data.table}, \code{collapse}, \code{kit} and \code{magrittr}) that 
#' provide native C/C++ code of proven quality, work well together, and enable complex statistical computing and data manipulation - with only \code{Rcpp} as an additional dependency. % (before v0.3.0 \code{matrixStats} and \code{fst} were also part of the core suite, but were removed following a poll in November 2022). 
#' % The package also harmonizes functionality among some of these core packages (see below). 
#' 
#' \code{fastverse} also allows users to freely (and permanently) extend or reduce the number of packages in the \emph{fastverse}. 
#' An overview of high-performing packages for various common tasks is provided in the \href{https://github.com/fastverse/fastverse#suggested-extensions}{README} file. 
#' An overview of the package and the different ways to extend the \emph{fastverse} is provided in the \href{https://fastverse.github.io/fastverse/articles/fastverse_intro.html}{vignette}. 
#' % Further functions help to determine joint dependencies, sort out namespace conflicts among attached packages, and update packages.  
#' 
#' @details NULL
#' @section Functions in the \code{fastverse} Package:
#' 
#' Functions to extend or reduce the number of packages in the \emph{fastverse} - either for the session or permanently - and to restore defaults. 
#'
#' \code{\link[=fastverse_extend]{fastverse_extend()}}\cr
#' \code{\link[=fastverse_detach]{fastverse_detach()}}\cr
#' \code{\link[=fastverse_reset]{fastverse_reset()}}
#' 
#' Function to display conflicts for \emph{fastverse} packages (or any other attached packages)
#'
#' \code{\link[=fastverse_conflicts]{fastverse_conflicts()}}
#'
#' Function to update \emph{fastverse} packages (and dependencies) and install (missing) packages
#' 
#' \code{\link[=fastverse_update]{fastverse_update()}}\cr
#' \code{\link[=fastverse_install]{fastverse_install()}}
#'
#' Utilities to retrieve the names of \emph{fastverse} packages (and dependencies), their update status, and produce a situation report
#' 
#' \code{\link[=fastverse_packages]{fastverse_packages()}}\cr
#' \code{\link[=fastverse_deps]{fastverse_deps()}}\cr
#' \code{\link[=fastverse_sitrep]{fastverse_sitrep()}}
#' 
#' Function to create a fully separate extensible meta-package/verse like \code{fastverse}
#' 
#' \code{\link[=fastverse_child]{fastverse_child()}}
#'
#' @section \emph{fastverse} Options:
#' \itemize{
#' \item \code{options(fastverse.quiet = TRUE)} will disable all automatic messages (including conflict reporting) when calling \code{library(fastvsers)}, \code{\link{fastverse_extend}}, \code{\link[=fastverse_update]{fastverse_update(install = TRUE)}} and \code{\link{fastverse_install}}.
#' \item \code{options(fastverse.styling = FALSE)} will disable all styling applied to text printed to the console. 
#' \item \code{options(fastverse.extend = c(...))} can be set before calling \code{library(fastvsers)} to extend the fastverse with some packages for the session. The same can be done with the
#' \code{\link{fastverse_extend}} function after \code{library(fastvsers)}, which will also populate \code{options("fastverse.extend")}. 
#' \item \code{options(fastverse.install = TRUE)} can be set before \code{library(fastverse)} to install any missing packages beforehand. See also \code{\link{fastverse_install}}.
#' }
#'
#' @section \emph{fastverse} Harmonizations:
#' % \itemize{
#' There are 2 internal clashes between \code{collapse::funique} and \code{kit::funique}, and \code{collapse::fdroplevels} and \code{data.table::fdroplevels}. % between \code{matrixStats::count} and \code{kit::count}.
#' The \emph{collapse} versions take precedence in both cases as they provide greater performance. % over the \emph{kit} versions. For a comparison of functionality see the details section of \code{\link{fastverse_conflicts}}.

#' % \item Quite a number of functions in the \emph{matrixStats} package do not (by default) preserve the attributes of objects passed to them, resulting in inconsistent behavior of different functions. 
#' % The GitHub version of the \code{fastverse} alters most of the functions where this is the case, listed in a global variable \code{.matrixStats_replaced}, bestowing them with 
#' % capabilities to preserve matrix dimension names and other attributes (for functions returning a matrix). This is done using very efficient R and C code, so that performance does not suffer. 
#' % When the \code{fastverse} is attached, these altered function are replaced in the \emph{matrixStats} namespace. Since CRAN does not allow namespace modifications in other packages, 
#' % this feature is only available in the GitHub version, installable using \code{remotes::install_github("fastverse/fastverse")}. Development of CRAN and GitHub version will
#' % continue synchronous until \emph{matrixStats} has evolved so that consistent attribute handling (\code{useNames = TRUE}) becomes the default.
#' % }
#' 
#' 
#' @docType package
#' @name fastverse
#' @aliases .matrixStats_replaced
#' 
#' 
#'
#' @importFrom utils stack packageVersion install.packages available.packages download.file unzip
#' @importFrom tools package_dependencies
#'
NULL


