is_attached <- function(x) paste0("package:", x) %in% search()



.onAttach <- function(...) {
  needed <- .core_pck[!is_attached(.core_pck)]
  if(length(needed) == 0L) return()
  
  fastverse_attach()
  
  if(!"package:conflicted" %in% search()) {
    x <- fastverse_conflicts()
    msg(fastverse_conflict_message(x), startup = TRUE)
  }
  
}
