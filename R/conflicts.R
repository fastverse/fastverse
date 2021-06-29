#' Conflicts between the fastverse and other packages
#'
#' This function lists all the conflicts between packages in the fastverse
#' and other packages that you have loaded.
#'
#' There is an internal conflict between \code{collapse::funique} and \code{kit::funique}. 
#' If both packages are unloaded, \code{collapse} is loaded before \code{kit}. In general the 
#' \code{collapse} version is often faster on data frames whereas the \code{kit} version is generally
#' faster for vectors and also supports matrices. 
#'
#' @export
#' @examples
#' fastverse_conflicts()
fastverse_conflicts <- function() {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- setNames(envs, envs)
  objs <- invert(lapply(envs, ls_env))
  
  conflicts <- objs[lengths(objs) > 1L]
  
  tidy_names <- paste0("package:", fastverse_packages())
  conflicts <- conflicts[vapply(conflicts, function(x) any(x %in% tidy_names), TRUE)]
  
  conflict_funs <- Map(confirm_conflict, conflicts, names(conflicts))
  conflict_funs <- conflict_funs[lengths(conflict_funs) > 0]
  
  structure(conflict_funs, class = "fastverse_conflicts")
}

fastverse_conflict_message <- function(x) {
  if (length(x) == 0L) return("")
  
  header <- rule(left = bold("Conflicts"), right = "fastverse_conflicts()")
  
  pkgs <- lapply(x, gsub, pattern = "^package:", replacement = "")
  others <- lapply(pkgs, `[`, -1L)
  other_calls <- mapply(function(x, y) paste0(blue(x), "::", y, "()", collapse = ", "), others, names(others))

  winner <- vapply(pkgs, `[`, character(1), 1L)
  funs <- format(paste0(blue(winner), "::", green(paste0(names(x), "()"))))
  bullets <- paste0(red("x"), " ", funs, " masks ", other_calls, collapse = "\n")
  
  paste0(header, "\n", bullets)
}

cat_line <- function(x) cat(x, "\n", sep = "", file = stdout(), append = TRUE)

#' @export
print.fastverse_conflicts <- function(x, ..., startup = FALSE) {
  cat_line(fastverse_conflict_message(x))
}

confirm_conflict <- function(packages, name) { # packages <- conflicts[[3]]; name <- names(conflicts[3])
  # Only look at functions
  objs <- lapply(packages, get, x = name) 
  objs <- objs[vapply(objs, is.function, TRUE)]
  if (length(objs) <= 1L) return()
  # Remove identical functions
  if (sum(!duplicated(objs)) == 1L) return()
  packages[!duplicated(packages)]
}

# Conflict exceptions...
ls_env <- function(env) {
  x <- ls(pos = env)
  if (identical(env, "package:kit")) {
    x <- setdiff(x, c("funique", "count"))
  } else if(identical(env, "package:collapse")) {
    x <- setdiff(x, "D")
  }
  x
}
