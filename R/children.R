

# pkgverse(pkg, pkgs, keep = FALSE, use = NULL, install_if = FALSE)
# name = "myverse"; core.pkgs = c("xts", "roll"); dir = "C:/Users/Sebastian Krantz/Documents/R"; keep.dir = TRUE
title = "A Cool Verse of Packages"
fastverse_child <- function(name, title, core.pkg, version = "0.1.0", dir = ".", install = TRUE, keep.dir = !install) {
  
  folder <- if(dir == "." || !nzchar(dir)) name else paste0(dir,  "/", name)
  zipfile <- paste0(folder, ".zip")
  
  # Download child branch of repo
  download.file(url = "https://github.com/SebKrantz/fastverse/archive/refs/heads/child.zip",
                destfile = zipfile)
  
  # Unzip the .zip file
  unzip(zipfile, junkpaths = TRUE, exdir = folder)
  file.remove(zipfile)
  
  # Substitute name and packages in the files
  files <- list.files(folder)  
  dir.create(paste0(folder, "/R"))
  dir.create(paste0(folder, "/man"))
  for(i in files) {
    pi <- paste0(folder, "/", i)
    fi <- gsub("fastversechild", name, readLines(pi), fixed = TRUE)
    if(i == "attach.R") fi[1L] <- paste0(".core_pkg <- c('", paste(core.pkg, collapse = "', '"), "')")
    if(i == "DESCRIPTION") {
      fi[2L] <- paste("Title:", title)
      fi[3L] <- paste("Version:", version)
    }
    fexti <- endsWith(i, c(".R", ".Rd"))
    writeLines(fi, paste0(folder, if(fexti[1L]) "/R/" else if(fexti[2L]) "/man/" else "/", i))
    if(any(fexti)) file.remove(pi)
  }
  
  # Install the package
  
  
}

