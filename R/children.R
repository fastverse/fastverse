#' Create a new (descendant) verse of packages
#' 
#' Creates and installs a fully customizable descendant verse of packages that is itself extensible and inherits 90\%
#' of the functionality of the \emph{fastverse} package.  
#' 
#' @param name character. The name of the child-verse e.g. 'myMLverse'. 
#' @param title character. The title of the child-verse e.g. 'My Machine Learning Verse'. 
#' @param pkg character. A vector of core packages for the new verse. 
#' @param maintainer character. A quoted \code{\link{person}} statement giving the package maintainer. 
#' If you do not intend to substantially modify the code of the resulting source package, \code{role = "cre"} is appropriate. See Examples. 
#' @param version character. A quoted version number for the new package. 
#' @param dir character. The directory in which a source directory dir/name for the package will be created. 
#' @param theme character. Set the colour-theme for text printed to the console. The options are \code{"fastverse"} or \code{"tidyverse"}.
#' @param install logical. \code{TRUE} installs the package using \code{install.packages(dir/name, repos = NULL, type = "source", ...)}. 
#' @param keep.dir logical. \code{FALSE} will remove the source directory (dir/name) again after installation. 
#' @param \dots further arguments to \code{\link{install.packages}}.
#' 
#' @details This function creates and installs a source package according to the users specification. 
#' For that it downloads the 'child' branch of the \href{https://github.com/SebKrantz/fastverse}{GitHub repository}, which 
#' was specifically set up to produce a new verse, unzips it into a source directory, and substitutes the user inputs into the files. 
#' The package is then installed from source, and (optionally) the source directory is removed again. 
#' 
#' \emph{fastverse} children inherit 90\% of the functionality of the \emph{fastverse} package: they are not permanently globally extensible and can not bear children themselves, 
#' but can be configured for projects (using a \code{.name} config file) and extended in the session.
#' Use of this function requires an internet connection but no additional R packages (like \code{devtools}, \code{remotes} or \code{roxygen2}).
#' 
#' @returns \code{fastverse_child} returns \code{NULL} invisibly. 
#' @export
#' @seealso \code{\link{fastverse_extend}}, \code{\link{fastverse}}
#' @examples \dontrun{
#' 
#' fastverse_child(
#'   name = "tsverse", 
#'   title = "Time Series Package Verse", 
#'   pkg = c("xts", "roll", "zoo", "tsbox", "urca", "tseries", "tsutils", "forecast"), 
#'   maintainer = 'person("GivenName", "FamilyName", role = "cre", email = "your@email.com")',
#'   dir = "C:/Users/.../Documents", 
#'   theme = "tidyverse")
#' }
fastverse_child <- function(name, title, pkg, maintainer, 
                            version = "0.1.0", dir = ".", 
                            theme = c("fastverse", "tidyverse"),
                            install = TRUE, keep.dir = TRUE, ...) {
  
  # Paths to folder and .zip file
  folder <- if(dir == "." || !nzchar(dir)) name else paste0(dir,  "/", name)
  zipfile <- paste0(folder, ".zip")
  
  # Download child branch of repo to .zip file
  download.file(url = "https://github.com/SebKrantz/fastverse/archive/refs/heads/child.zip",
                destfile = zipfile)
  
  # Unzip the .zip file
  unzip(zipfile, junkpaths = TRUE, exdir = folder)
  file.remove(zipfile)
  
  # Substitute user information in the files
  tv <- match.arg(theme) == "tidyverse"
  files <- list.files(folder)  
  dir.create(paste0(folder, "/R"))
  dir.create(paste0(folder, "/man"))
  for(i in setdiff(files, "LICENSE")) { # print(i)
    pi <- paste0(folder, "/", i)
    fi <- readLines(pi)
    fexti <- endsWith(i, c(".R", ".Rd"))
    if(tv && fexti[1L]) {
      if(i == "update.R") {
        fi <- gsub("magenta2(package_pad)", "package_pad", fi, fixed = TRUE)
        fi <- gsub('kingsblue("fastversechild")', '"fastversechild"', fi, fixed = TRUE)
        fi <- gsub('gold(format(behind$package))', 'format(behind$package)', fi, fixed = TRUE)
      } else if(i == "attach.R") 
        fi <- gsub('kingsblue("fastversechild")', 'text_col("fastversechild")', fi, fixed = TRUE)
      fi <- gsub("magenta2(", "blue(", fi, fixed = TRUE)
      fi <- gsub("kingsblue(", "green(", fi, fixed = TRUE)
      fi <- gsub("gold(", "yellow(", fi, fixed = TRUE)
    }
    fi <- gsub("fastversechild", name, fi, fixed = TRUE)
    if(i == "attach.R") fi[1L] <- paste0(".core_pkg <- c('", paste(pkg, collapse = "', '"), "')")
    if(i == "DESCRIPTION") {
      fi[2L] <- paste("Title:", title)
      fi[3L] <- paste("Version:", version)
      fi[4L] <- paste0("Authors@R: c(", maintainer, ",")
      fi[startsWith(fi, "Imports:")] <- paste("Imports:", paste(pkg, collapse = ", "))
    }
    if(startsWith(i, "fastverse")) {
      file.remove(pi)
      i <- sub("fastverse|fastversechild", name, i)
    } else if(any(fexti)) file.remove(pi)
    writeLines(fi, paste0(folder, if(fexti[1L]) "/R/" else if(fexti[2L]) "/man/" else "/", i))
  }
  
  # Install the package
  if(install) install.packages(folder, repos = NULL, type = "source", ...)
  
  # Remove directory
  if(!keep.dir) unlink(folder, recursive = TRUE)
  
  invisible()
}

