is_attached <- function(x) paste0("package:", x) %in% search()



.onAttach <- function(...) {
  
  ext_pck_file <- paste(system.file(package = 'fastverse'), 'fastverse_ext_pck.txt', sep = '/')
  
  if(file.exists(ext_pck_file)) {
    file_pck <- readLines(file(ext_pck_file))
    needed <- file_pck[!is_attached(file_pck)]
  } else {
    needed <- .core_pck[!is_attached(.core_pck)]
  }
  
  if(length(needed) == 0L) return()
  
  fastverse_attach(needed)
  
  if(!"package:conflicted" %in% search()) {
    x <- fastverse_conflicts()
    msg(fastverse_conflict_message(x), startup = TRUE)
  }
  
}
