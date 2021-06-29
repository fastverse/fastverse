.core_pck <- c("data.table", "magrittr", "kit", "collapse", "matrixStats", "fst")


core_unloaded <- function() {
  search <- paste0("package:", .core_pck)
  .core_pck[!search %in% search()]
}

# Attach the package from the same package library it was
# loaded from before. https://github.com/fastverse/fastverse/issues/171
same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  do.call("library", list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE))
}

tick <- "\U2713" # Heavy: \U2714 # cli::symbol$tick # Load from RDA file...

# col_align <- function(x) {
#   ncx <- nchar(x)
#   max_ncx <- max(ncx)
#   spaces <- vapply(max_ncx - ncx, function(x) paste(rep(" ", x), collapse = ""), character(1), USE.NAMES = FALSE)
#   paste0(x, spaces)
# }

fastverse_attach <- function() {
  to_load <- core_unloaded()
  if(length(to_load) == 0L) return(invisible())
  
  msg(rule(left = bold("Attaching packages"),
           right = paste0(kingsblue("fastverse "), package_version("fastverse"))), 
      startup = TRUE)
  
  versions <- vapply(to_load, package_version, character(1))
  packages <- paste0(kingsblue(tick), " ", magenta2(format(to_load)), " ", grey09(format(versions)))
  
  if(length(packages) %% 2L == 1L) packages <- append(packages, "")
  
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])
  
  msg(paste(info, collapse = "\n"), startup = TRUE) # cat(paste(info, collapse = "\n"))
  
  oldopts <- options(warn = -1)
  on.exit(options(oldopts))
  
  suppressPackageStartupMessages({
    lapply(setdiff(to_load, "matrixStats"), same_library)
    if(!any("matrixStats" == to_load)) detach("package:matrixStats", unload = TRUE)
    replace_matrixStats()
  })
  
  invisible()
}

#' @export
fastverse_detach <- function(unload = TRUE, include.self = TRUE) {
  loaded <- setdiff(.core_pck, core_unloaded())
  if(include.self) loaded <- c(loaded, "fastverse")
  if(length(loaded)) {
    ul <- paste0("package:", loaded) 
    for(i in ul)  eval(substitute(detach(pck, unload = unload), list(pck = i)))
  }
}
