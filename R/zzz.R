
.onAttach <- function(...) {
  
  needed <- ckeck_attached()
  
  if(length(needed) == 0L) return()
  
  fastverse_attach(needed, onattach = TRUE)
  
  if(!"package:conflicted" %in% search()) {
    x <- fastverse_conflicts() # TODO: Speed up.
    msg(fastverse_conflict_message(x), startup = TRUE)
  }
  
}
