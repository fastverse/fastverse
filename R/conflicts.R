# Conflict exceptions...
ls_env <- function(env) {
  x <- ls(pos = env)
  switch(env, 
         `package:kit` = setdiff(x, c("funique", "count")),
         `package:collapse` = setdiff(x, "D"),
         x)
}

# Significantly faster than what was previously there
invert_simplify <- function(x) {
  if (length(x) == 0L) return()
  stacked <- unclass(utils::stack(x))
  dup <- stacked$values[duplicated(stacked$values)]
  if(!length(dup)) return()
  dup <- which(stacked$values %in% dup)
  stacked <- lapply(stacked, `[`, dup)
  split(as.character(stacked$ind), stacked$values)
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


#' Conflicts between the fastverse and other packages
#'
#' This function lists all the conflicts among \emph{fastverse} packages and between \emph{fastverse} packages and other attached packages.
#' It can also be used to check conflicts for any other attached packages.
#'
#'
#' There are 2 internal conflict in the core \emph{fastverse} which are not displayed by \code{fastverse_conflicts()}:
#' \itemize{
#' \item \code{collapse::funique} maks \code{kit::funique}. If both packages are unloaded, \emph{collapse} is loaded after \emph{kit}. In general, the 
#' \emph{collapse} version is often faster on data frames and supports unique rows on selected columns, but only preserves the first-match appearance order of 
#' character data (unique values for numeric columns are determined using a sort-based method). 
#' An option \code{sort = TRUE} lets \code{collapse::funique} return sorted unique values (for both character and numeric data). 
#' The \emph{kit} version is generally faster for vectors and someties for data frames, and also supports matrices. 
#' It is a full hash-table based implementation that always preserves the first-match appearance order of data.   
#' 
#' \item \code{matrixStats::count} masks \code{kit::count}. The \emph{matrixStats} version is more flexible, supporting restricted search and missing value removal. The \emph{kit} version is nearly twice as fast. 
#' }
#' @param pck character. A vector of packages to check conflicts for. The default is all \emph{fastverse} packages. 
#' 
#' @seealso \code{\link{fastverse}}
#' @export
#' @examples
#' # Check conflicts between fastverse packages and all attached packages
#' fastverse_conflicts()
#' 
#' # Check conflicts among all attached packages
#' fastverse_conflicts(rm_stub(search()[-1], "package:"))
fastverse_conflicts <- function(pck = fastverse_packages(include.self = FALSE)) {
  envs <- grep("^package:", search(), value = TRUE)
  names(envs) <- envs
  conflicts <- invert_simplify(lapply(envs, ls_env)) # formerly objs
  # conflicts <- objs[as.logical(lengths(objs))] # Redundant: if envs is non empty, objs will also be
  tidy_names <- paste0("package:", pck)
  conflicts <- conflicts[vapply(conflicts, function(x) any(x %in% tidy_names), TRUE)]
  
  conflict_funs <- Map(confirm_conflict, conflicts, names(conflicts))
  conflict_funs <- conflict_funs[lengths(conflict_funs) > 0L]
  
  structure(conflict_funs, class = "fastverse_conflicts")
}

fastverse_conflict_message <- function(x) {
  if (length(x) == 0L) return("")
  
  header <- text_col(rule(left = "Conflicts", style.left = bold, right = "fastverse_conflicts()"))
  
  pkgs <- lapply(x, gsub, pattern = "^package:", replacement = "")
  others <- lapply(pkgs, `[`, -1L)
  other_calls <- mapply(function(x, y) paste0(blue(x), text_col(paste0("::", y, "()")), collapse = ", "), others, names(others))

  winner <- vapply(pkgs, `[`, character(1L), 1L)
  funs <- format(paste0(blue(winner), text_col("::"), green(paste0(names(x), "()"))))
  bullets <- paste0(red("x"), " ", funs, text_col(" masks "), other_calls, collapse = "\n")
  
  paste0(header, "\n", bullets)
}

cat_line <- function(x) cat(x, "\n", sep = "", file = stdout(), append = TRUE)

#' @export
print.fastverse_conflicts <- function(x, ..., startup = FALSE) {
  if(length(x)) cat_line(fastverse_conflict_message(x))
  # fastverse_conflict_message(x) why cat_line ??
}


