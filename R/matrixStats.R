#################################################################
# Add attribute handling capabilities to 'matrixStats' 
# functions where is is not the case. 
# ---------------------------------------------------------------
# This is done in line with attribute handling 
# principles adopted in 'collapse' stipulating that 
# essentially all attributes are copied as long as the object 
# remains a matrix, and row/column names are suitably
# modified. This means that we can call colCumsums()
# on a time-series matrix such as 'EuStockMarkets'
# and retain the attributes of that object. If the object
# has a class (is.object(x) is true) and we reduce the 
# number of rows, e.g. colCumsums(EuStockMarkets, rows = 1:3), 
# Then dimnames are handled but other attributes are not copied. 
# See NEWS for 'collapse' 1.4.0 for attribute handling guidelines 
# I adopted that appear optimal for the bulk of R objects.
##################################################################

# Export 2 C functions that help with attribute handling
#' @useDynLib fastverse, .registration = TRUE
NULL

# These functions already handle attributes...
ms_handles_attr <- c("colWeightedMeans", "colWeightedMedians", "rowWeightedMedians",
                     "colWeightedVars", "rowWeightedVars", "colVarDiffs", "rowVarDiffs",
                     "colWeightedSds", "rowWeightedSds", "colSdDiffs", "rowSdDiffs",
                     "colWeightedMads", "rowWeightedMads", "colMadDiffs", "rowMadDiffs",
                     "colQuantiles", "rowQuantiles", "colIQRDiffs", "rowIQRDiffs",
                     "colLogSumExps", "rowLogSumExps")

# These don't but it is difficult to implement it, and they are not widely used
ms_difficult <- c("colAvgsPerRowSet", "rowAvgsPerColSet", "colCollapse", "rowCollapse", "colTabulates", "rowTabulates")

# Do: 
# x_OP_y
# t_tx_OP_y

ms <- if(requireNamespace("matrixStats", quietly = TRUE)) getNamespace("matrixStats") else new.env()

# These should all be vector-based functions..
# rem <- setdiff(ls(ms), c(ls(), ms_handles_attr, ms_difficult))

rowWeightedMeans_ms <- get0("rowWeightedMeans", envir = ms)
# #' @export
rowWeightedMeans <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowWeightedMeans_ms(x, w, rows, cols, na.rm, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn # if(is.character(rows)) ckmatch(rows, rn) else -> Error for some...
  res
}

colMeans2_ms <- get0("colMeans2", envir = ms)
# #' @export
colMeans2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMeans2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn # if(is.character(cols)) ckmatch(cols, cn) else -> error for some
  res
}

rowMeans2_ms <- get0("rowMeans2", envir = ms)
# #' @export
rowMeans2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMeans2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colSums2_ms <- get0("colSums2", envir = ms)
# #' @export
colSums2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colSums2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn # if(is.character(cols)) ckmatch(cols, cn) else -> error for some
  res
}

rowSums2_ms <- get0("rowSums2", envir = ms)
# #' @export
rowSums2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowSums2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

# Note: The function actually uses plain R 
colProds_ms <- get0("colProds", envir = ms)
# #' @export
colProds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, method = c("direct", "expSumLog"), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colProds_ms(x, rows, cols, na.rm, method, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

# Note: The function actually uses plain R 
rowProds_ms <- get0("rowProds", envir = ms)
# #' @export
rowProds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, method = c("direct", "expSumLog"), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowProds_ms(x, rows, cols, na.rm, method, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colMedians_ms <- get0("colMedians", envir = ms)
# #' @export
colMedians <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMedians_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMedians_ms <- get0("rowMedians", envir = ms)
# #' @export
rowMedians <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMedians_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colVars_ms <- get0("colVars", envir = ms)
# #' @export
colVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colVars_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowVars_ms <- get0("rowVars", envir = ms)
# #' @export
rowVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowVars_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colSds_ms <- get0("colSds", envir = ms)
# #' @export
colSds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colSds_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowSds_ms <- get0("rowSds", envir = ms)
# #' @export
rowSds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowSds_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colMads_ms <- get0("colMads", envir = ms)
# #' @export
colMads <- function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMads_ms(x, rows, cols, center, constant, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMads_ms <- get0("rowMads", envir = ms)
# #' @export
rowMads <- function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMads_ms(x, rows, cols, center, constant, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colIQRs_ms <- get0("colIQRs", envir = ms)
# #' @export
colIQRs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colIQRs_ms(x, rows, cols, na.rm, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowIQRs_ms <- get0("rowIQRs", envir = ms)
# #' @export
rowIQRs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowIQRs_ms(x, rows, cols, na.rm, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colRanges_ms <- get0("colRanges", envir = ms)
# #' @export
colRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colRanges_ms(x, rows, cols, na.rm, dim., ...)
  dimnames(res) <- list(if(length(cols)) cn[cols] else cn, c("Min", "Max"))
  res
}

rowRanges_ms <- get0("rowRanges", envir = ms)
# #' @export
rowRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowRanges_ms(x, rows, cols, na.rm, dim., ...)
  dimnames(res) <- list(if(length(rows)) rn[rows] else rn, c("Min", "Max")) 
  res
}

colMins_ms <- get0("colMins", envir = ms)
# #' @export
colMins <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMins_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMins_ms <- get0("rowMins", envir = ms)
# #' @export
rowMins <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMins_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colMaxs_ms <- get0("colMaxs", envir = ms)
# #' @export
colMaxs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMaxs_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMaxs_ms <- get0("rowMaxs", envir = ms)
# #' @export
rowMaxs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMaxs_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colOrderStats_ms <- get0("colOrderStats", envir = ms)
# #' @export
colOrderStats <- function(x, rows = NULL, cols = NULL, which, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colOrderStats_ms(x, rows, cols, which, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowOrderStats_ms <- get0("rowOrderStats", envir = ms)
# #' @export
rowOrderStats <- function(x, rows = NULL, cols = NULL, which, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowOrderStats_ms(x, rows, cols, which, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAnyMissings_ms <- get0("colAnyMissings", envir = ms)
# #' @export
colAnyMissings <- function(x, rows = NULL, cols = NULL, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnyMissings_ms(x, rows, cols, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnyMissings_ms <- get0("rowAnyMissings", envir = ms)
# #' @export
rowAnyMissings <- function(x, rows = NULL, cols = NULL, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnyMissings_ms(x, rows, cols, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAnyNAs_ms <- get0("colAnyNAs", envir = ms)
# #' @export
colAnyNAs <- function(x, rows = NULL, cols = NULL, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnyNAs_ms(x, rows, cols, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnyNAs_ms <- get0("rowAnyNAs", envir = ms)
# #' @export
rowAnyNAs <- function(x, rows = NULL, cols = NULL, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnyNAs_ms(x, rows, cols, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAnys_ms <- get0("colAnys", envir = ms)
# #' @export
colAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnys_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnys_ms <- get0("rowAnys", envir = ms)
# #' @export
rowAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnys_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAlls_ms <- get0("colAlls", envir = ms)
# #' @export
colAlls <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAlls_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAlls_ms <- get0("rowAlls", envir = ms)
# #' @export
rowAlls <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAlls_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colCounts_ms <- get0("colCounts", envir = ms)
# #' @export
colCounts <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colCounts_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowCounts_ms <- get0("rowCounts", envir = ms)
# #' @export
rowCounts <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowCounts_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}


# Unit: nanoseconds
#                                        expr  min   lq     mean median   uq     max neval cld
#               dimnames(m)[[2L]] <- dn[[2L]] 1338 1785 2201.520   1785 2231 8176593 1e+05  b 
#     dimnames(m) <- list(dn[[1L]], dn[[2L]])  892 1339 1620.331   1339 1785  131643 1e+05 a  
# dimnames(m) <- pairlist(dn[[1L]], dn[[2L]]) 2231 2678 3631.340   3124 3570 5464301 1e+05   c

colCumsums_ms <- get0("colCumsums", envir = ms)
# #' @export
colCumsums <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  dn <- dimnames(x)
  res <- colCumsums_ms(x, rows, cols, dim., ...)
  if(length(dn)) { # For efficiency I assume here that if a matrix does not have dimnames, nothing else of interest is attached...
    if(length(rows)) {
      if(!is.object(x)) .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]][rows], if(length(cols)) dn[[2L]][cols] else dn[[2L]])
    } else if(length(cols)) {
      .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]], dn[[2L]][cols])
    } else .Call(C_DUPLICATE_ATTRIB, res, x)
  }
  res
}

rowCumsums_ms <- get0("rowCumsums", envir = ms)
# #' @export
rowCumsums <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  dn <- dimnames(x)
  res <- rowCumsums_ms(x, rows, cols, dim., ...)
  if(length(dn)) {
    if(length(rows)) {
      if(!is.object(x)) .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]][rows], if(length(cols)) dn[[2L]][cols] else dn[[2L]])
    } else if(length(cols)) {
      .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]], dn[[2L]][cols])
    } else .Call(C_DUPLICATE_ATTRIB, res, x)
  }
  res
}

colCumprods_ms <- get0("colCumprods", envir = ms)
# #' @export
colCumprods <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  dn <- dimnames(x)
  res <- colCumprods_ms(x, rows, cols, dim., ...)
  if(length(dn)) {
    if(length(rows)) {
      if(!is.object(x)) .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]][rows], if(length(cols)) dn[[2L]][cols] else dn[[2L]])
    } else if(length(cols)) {
      .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]], dn[[2L]][cols])
    } else .Call(C_DUPLICATE_ATTRIB, res, x)
  }
  res
}

rowCumprods_ms <- get0("rowCumprods", envir = ms)
# #' @export
rowCumprods <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  dn <- dimnames(x)
  res <- rowCumprods_ms(x, rows, cols, dim., ...)
  if(length(dn)) {
    if(length(rows)) {
      if(!is.object(x)) .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]][rows], if(length(cols)) dn[[2L]][cols] else dn[[2L]])
    } else if(length(cols)) {
      .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]], dn[[2L]][cols])
    } else .Call(C_DUPLICATE_ATTRIB, res, x)
  }
  res
}

colCummins_ms <- get0("colCummins", envir = ms)
# #' @export
colCummins <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  dn <- dimnames(x)
  res <- colCummins_ms(x, rows, cols, dim., ...)
  if(length(dn)) {
    if(length(rows)) {
      if(!is.object(x)) .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]][rows], if(length(cols)) dn[[2L]][cols] else dn[[2L]])
    } else if(length(cols)) {
      .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]], dn[[2L]][cols])
    } else .Call(C_DUPLICATE_ATTRIB, res, x)
  }
  res
}

rowCummins_ms <- get0("rowCummins", envir = ms)
# #' @export
rowCummins <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  dn <- dimnames(x)
  res <- rowCummins_ms(x, rows, cols, dim., ...)
  if(length(dn)) {
    if(length(rows)) {
      if(!is.object(x)) .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]][rows], if(length(cols)) dn[[2L]][cols] else dn[[2L]])
    } else if(length(cols)) {
      .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]], dn[[2L]][cols])
    } else .Call(C_DUPLICATE_ATTRIB, res, x)
  }
  res
}

colCummaxs_ms <- get0("colCummaxs", envir = ms)
# #' @export
colCummaxs <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  dn <- dimnames(x)
  res <- colCummaxs_ms(x, rows, cols, dim., ...)
  if(length(dn)) {
    if(length(rows)) {
      if(!is.object(x)) .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]][rows], if(length(cols)) dn[[2L]][cols] else dn[[2L]])
    } else if(length(cols)) {
      .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]], dn[[2L]][cols])
    } else .Call(C_DUPLICATE_ATTRIB, res, x)
  }
  res
}

rowCummaxs_ms <- get0("rowCummaxs", envir = ms)
# #' @export
rowCummaxs <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  dn <- dimnames(x)
  res <- rowCummaxs_ms(x, rows, cols, dim., ...)
  if(length(dn)) {
    if(length(rows)) {
      if(!is.object(x)) .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]][rows], if(length(cols)) dn[[2L]][cols] else dn[[2L]])
    } else if(length(cols)) {
      .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]], dn[[2L]][cols])
    } else .Call(C_DUPLICATE_ATTRIB, res, x)
  }
  res
}

colRanks_ms <- get0("colRanks", envir = ms)
# #' @export
colRanks <- function(x, rows = NULL, cols = NULL, 
                     ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"),
                     dim. = dim(x), preserveShape = FALSE, ...) {
  dn <- dimnames(x)
  res <- colRanks_ms(x, rows, cols, ties.method, dim., preserveShape, ...)
  if(length(dn)) {
    if(length(rows)) {
      if(!is.object(x)) .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- if(preserveShape) 
        list(dn[[1L]][rows], if(length(cols)) dn[[2L]][cols] else dn[[2L]]) else 
          list(if(length(cols)) dn[[2L]][cols] else dn[[2L]], dn[[1L]][rows])
    } else if(length(cols)) {
      .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- if(preserveShape) list(dn[[1L]], dn[[2L]][cols]) else list(dn[[2L]][cols], dn[[1L]])
    } else {
      .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- if(preserveShape) dn else dn[2:1]
    }
  }
  res
}

rowRanks_ms <- get0("rowRanks", envir = ms)
# #' @export
rowRanks <- function(x, rows = NULL, cols = NULL, 
                     ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"), 
                     dim. = dim(x), ...) {
  dn <- dimnames(x)
  res <- rowRanks_ms(x, rows, cols, ties.method, dim., ...)
  if(length(dn)) {
    if(length(rows)) {
      if(!is.object(x)) .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]][rows], if(length(cols)) dn[[2L]][cols] else dn[[2L]])
    } else if(length(cols)) {
      .Call(C_copyMostAttrib, res, x)
      dimnames(res) <- list(dn[[1L]], dn[[2L]][cols])
    } else .Call(C_DUPLICATE_ATTRIB, res, x)
  }
  res
}

colDiffs_ms <- get0("colDiffs", envir = ms)
# #' @export
colDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colDiffs_ms(x, rows, cols, lag, differences, dim., ...)
  if(length(cn)) dimnames(res) <- list(NULL, if(length(cols)) cn[cols] else cn)
  res
}

rowDiffs_ms <- get0("rowDiffs", envir = ms)
# #' @export
rowDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowDiffs_ms(x, rows, cols, lag, differences, dim., ...)
  if(length(rn)) dimnames(res) <- list(if(length(rows)) rn[rows] else rn, NULL)
  res
}

#' @export
.matrixStats_replaced <- c("colAlls", "colAnyMissings", "colAnyNAs", "colAnys", "colCounts", 
                           "colCummaxs", "colCummins", "colCumprods", "colCumsums", 
                           "colDiffs", "colIQRs", "colMads", "colMaxs", "colMeans2", "colMedians", 
                           "colMins", "colOrderStats", "colProds", "colRanges", "colRanks", "colSds", 
                           "colSums2", "colVars", "rowAlls", "rowAnyMissings", "rowAnyNAs", "rowAnys", 
                           "rowCounts", "rowCummaxs", "rowCummins", "rowCumprods", "rowCumsums", 
                           "rowDiffs", "rowIQRs", "rowMads", "rowMaxs", "rowMeans2", "rowMedians", 
                           "rowMins", "rowOrderStats", "rowProds", "rowRanges", "rowRanks", "rowSds", 
                           "rowSums2", "rowVars", "rowWeightedMeans")
  
# .matrixStats_replaced <- grep("^row|^col", setdiff(ls(ms), c(ms_handles_attr, ms_difficult)), value = TRUE)


# https://stackoverflow.com/questions/24331690/modify-package-function?noredirect=1&lq=1
# https://stackoverflow.com/questions/3094232/add-objects-to-package-namespace
replace_matrixStats <- function() {
  for(i in .matrixStats_replaced) {
    unlockBinding(as.name(i), env = ms)
    assign(i, get(i), envir = ms)
  }
  environment(C_copyMostAttrib) <- ms
  environment(C_DUPLICATE_ATTRIB) <- ms
  attachNamespace(ms)
  rm(ms, i)
}

