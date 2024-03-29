% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update.R
\name{fastverse_install}
\alias{fastverse_install}
\title{Install (missing) fastverse packages}
\usage{
fastverse_install(
  ...,
  only.missing = TRUE,
  install = TRUE,
  repos = getOption("repos")
)
}
\arguments{
\item{\dots}{comma-separated package names, quoted or unquoted, or vectors of package names. If left empty, all packages returned by \code{\link{fastverse_packages}} are checked.}

\item{only.missing}{logical. \code{TRUE} only installs packages that are unavailable. \code{FALSE} installs all packages, even if they are available.}

\item{install}{logical. \code{TRUE} will proceed to install packages, whereas \code{FALSE} (recommended) will print the installation command asking you to run it in a clean R session.}

\item{repos}{character vector. Base URL(s) of the repositories to use, e.g., the URL of a CRAN mirror such as \code{"https://cloud.r-project.org"}. 
The macro \code{.fastverse_repos} contains the URL of the \href{https://fastverse.r-universe.dev}{fastverse r-universe server} to check/install the development version of packages.}
}
\value{
\code{fastverse_install} returns \code{NULL} invisibly.
}
\description{
This function (by default) checks if any \emph{fastverse} package is missing and installs the missing package(s). The development versions of \emph{fastverse} packages can also be installed from \href{https://fastverse.r-universe.dev}{r-universe}. The link to the repository is contained in the \code{.fastverse_repos} macro.
}
\note{
There is also the possibility to set \code{options(fastverse.install = TRUE)} before \code{library(fastverse)}, which will call \code{fastverse_install()} before loading any packages to make sure all packages are available.
If you are using a \code{.fastverse} configuration file inside a project (see vignette), you can also place \code{_opt_fastverse.install = TRUE} before the list of packages in that file.
}
\seealso{
\code{\link{fastverse_update}}, \code{\link{fastverse}}
}
