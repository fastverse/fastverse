#' The fastversechild
#' 
#' @description
#' The \emph{fastversechild} is an extensible suite of R packages. It is a descendant of the \href{https://sebkrantz.github.io/fastverse/}{fastverse}.
#' 
#' @details NULL
#' @section Functions in the \emph{fastversechild} Package:
#' 
#' Functions to extend or reduce the number of packages in the \emph{fastversechild}
#'
#' \code{\link[=fastversechild_extend]{fastversechild_extend()}}\cr
#' \code{\link[=fastversechild_detach]{fastversechild_detach()}}
#' 
#' Function to display conflicts for \emph{fastversechild} packages (or any other attached packages)
#'
#' \code{\link[=fastversechild_conflicts]{fastversechild_conflicts()}}
#'
#' Function to update \emph{fastversechild} packages (and dependencies) and install (missing) packages
#' 
#' \code{\link[=fastversechild_update]{fastversechild_update()}}\cr
#' \code{\link[=fastversechild_install]{fastversechild_install()}}
#'
#' Utilities to retrieve the names of \emph{fastversechild} packages (and dependencies), their update status and produce a situation report.
#' 
#' \code{\link[=fastversechild_packages]{fastversechild_packages()}}\cr
#' \code{\link[=fastversechild_deps]{fastversechild_deps()}}\cr
#' \code{\link[=fastversechild_sitrep]{fastversechild_sitrep()}}
#'
#' @section \emph{fastversechild} Options:
#' \itemize{
#' \item \code{options(fastversechild.quiet = TRUE)} will disable all automatic messages (including conflict reporting) when calling \code{library(fastversechild)}, \code{\link{fastversechild_extend}}, \code{\link[=fastversechild_update]{fastversechild_update(install = TRUE)}} and \code{\link{fastversechild_install}}.
#' \item \code{options(fastversechild.styling = FALSE)} will disable all styling applied to text printed to the console. 
#' \item \code{options(fastversechild.extend = c(...))} can be set before calling \code{library(fastversechild)} to extend the fastversechild with some packages for the session. The same can be done with the
#' \code{\link{fastversechild_extend}} function after \code{library(fastversechild)}, which will also populate \code{options("fastversechild.extend")}. 
#' \item \code{options(fastversechild.install = TRUE)} can be set before \code{library(fastversechild)} to install any missing packages beforehand. See also \code{\link{fastversechild_install}}.
#' }
#'
#' @docType package
#' @name fastversechild
#' 
#' @importFrom utils stack packageVersion install.packages available.packages
#' @importFrom tools package_dependencies
#'
NULL


