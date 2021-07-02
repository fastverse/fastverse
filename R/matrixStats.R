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

# These should all be vector-based functions..
# rem <- setdiff(ls("package:matrixStats"), c(ls(), ms_handles_attr, ms_difficult))

rowWeightedMeans_ms <- matrixStats::rowWeightedMeans
# #' @export
rowWeightedMeans <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowWeightedMeans_ms(x, w, rows, cols, na.rm, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn # if(is.character(rows)) ckmatch(rows, rn) else -> Error for some...
  res
}

colMeans2_ms <- matrixStats::colMeans2
# #' @export
colMeans2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMeans2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn # if(is.character(cols)) ckmatch(cols, cn) else -> error for some
  res
}

rowMeans2_ms <- matrixStats::rowMeans2
# #' @export
rowMeans2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMeans2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colSums2_ms <- matrixStats::colSums2
# #' @export
colSums2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colSums2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn # if(is.character(cols)) ckmatch(cols, cn) else -> error for some
  res
}

rowSums2_ms <- matrixStats::rowSums2
# #' @export
rowSums2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowSums2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

# Note: The function actually uses plain R 
colProds_ms <- matrixStats::colProds
# #' @export
colProds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, method = c("direct", "expSumLog"), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colProds_ms(x, rows, cols, na.rm, method, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

# Note: The function actually uses plain R 
rowProds_ms <- matrixStats::rowProds
# #' @export
rowProds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, method = c("direct", "expSumLog"), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowProds_ms(x, rows, cols, na.rm, method, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colMedians_ms <- matrixStats::colMedians
# #' @export
colMedians <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMedians_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMedians_ms <- matrixStats::rowMedians
# #' @export
rowMedians <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMedians_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colVars_ms <- matrixStats::colVars
# #' @export
colVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colVars_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowVars_ms <- matrixStats::rowVars
# #' @export
rowVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowVars_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colSds_ms <- matrixStats::colSds
# #' @export
colSds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colSds_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowSds_ms <- matrixStats::rowSds
# #' @export
rowSds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowSds_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colMads_ms <- matrixStats::colMads
# #' @export
colMads <- function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMads_ms(x, rows, cols, center, constant, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMads_ms <- matrixStats::rowMads
# #' @export
rowMads <- function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMads_ms(x, rows, cols, center, constant, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colIQRs_ms <- matrixStats::colIQRs
# #' @export
colIQRs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colIQRs_ms(x, rows, cols, na.rm, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowIQRs_ms <- matrixStats::rowIQRs
# #' @export
rowIQRs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowIQRs_ms(x, rows, cols, na.rm, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colRanges_ms <- matrixStats::colRanges
# #' @export
colRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colRanges_ms(x, rows, cols, na.rm, dim., ...)
  dimnames(res) <- list(if(length(cols)) cn[cols] else cn, c("Min", "Max"))
  res
}

rowRanges_ms <- matrixStats::rowRanges
# #' @export
rowRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowRanges_ms(x, rows, cols, na.rm, dim., ...)
  dimnames(res) <- list(if(length(rows)) rn[rows] else rn, c("Min", "Max")) 
  res
}

colMins_ms <- matrixStats::colMins
# #' @export
colMins <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMins_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMins_ms <- matrixStats::rowMins
# #' @export
rowMins <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMins_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colMaxs_ms <- matrixStats::colMaxs
# #' @export
colMaxs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMaxs_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMaxs_ms <- matrixStats::rowMaxs
# #' @export
rowMaxs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMaxs_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colOrderStats_ms <- matrixStats::colOrderStats
# #' @export
colOrderStats <- function(x, rows = NULL, cols = NULL, which, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colOrderStats_ms(x, rows, cols, which, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowOrderStats_ms <- matrixStats::rowOrderStats
# #' @export
rowOrderStats <- function(x, rows = NULL, cols = NULL, which, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowOrderStats_ms(x, rows, cols, which, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAnyMissings_ms <- matrixStats::colAnyMissings
# #' @export
colAnyMissings <- function(x, rows = NULL, cols = NULL, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnyMissings_ms(x, rows, cols, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnyMissings_ms <- matrixStats::rowAnyMissings
# #' @export
rowAnyMissings <- function(x, rows = NULL, cols = NULL, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnyMissings_ms(x, rows, cols, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAnyNAs_ms <- matrixStats::colAnyNAs
# #' @export
colAnyNAs <- function(x, rows = NULL, cols = NULL, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnyNAs_ms(x, rows, cols, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnyNAs_ms <- matrixStats::rowAnyNAs
# #' @export
rowAnyNAs <- function(x, rows = NULL, cols = NULL, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnyNAs_ms(x, rows, cols, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAnys_ms <- matrixStats::colAnys
# #' @export
colAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnys_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnys_ms <- matrixStats::rowAnys
# #' @export
rowAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnys_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAlls_ms <- matrixStats::colAlls
# #' @export
colAlls <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAlls_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAlls_ms <- matrixStats::rowAlls
# #' @export
rowAlls <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAlls_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colCounts_ms <- matrixStats::colCounts
# #' @export
colCounts <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colCounts_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowCounts_ms <- matrixStats::rowCounts
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

colCumsums_ms <- matrixStats::colCumsums
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

rowCumsums_ms <- matrixStats::rowCumsums
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

colCumprods_ms <- matrixStats::colCumprods
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

rowCumprods_ms <- matrixStats::rowCumprods
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

colCummins_ms <- matrixStats::colCummins
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

rowCummins_ms <- matrixStats::rowCummins
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

colCummaxs_ms <- matrixStats::colCummaxs
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

rowCummaxs_ms <- matrixStats::rowCummaxs
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

colRanks_ms <- matrixStats::colRanks
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

rowRanks_ms <- matrixStats::rowRanks
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

colDiffs_ms <- matrixStats::colDiffs
# #' @export
colDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colDiffs_ms(x, rows, cols, lag, differences, dim., ...)
  if(length(cn)) dimnames(res) <- list(NULL, if(length(cols)) cn[cols] else cn)
  res
}

rowDiffs_ms <- matrixStats::rowDiffs
# #' @export
rowDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowDiffs_ms(x, rows, cols, lag, differences, dim., ...)
  if(length(rn)) dimnames(res) <- list(if(length(rows)) rn[rows] else rn, NULL)
  res
}

ms <- getNamespace("matrixStats")

ms_replaced <- grep("^row|^col", setdiff(ls(ms), c(ms_handles_attr, ms_difficult)), value = TRUE)

# https://stackoverflow.com/questions/24331690/modify-package-function?noredirect=1&lq=1
# https://stackoverflow.com/questions/3094232/add-objects-to-package-namespace
replace_matrixStats <- function() {
  for(i in ms_replaced) {
    # if(!endsWith(i, "_ms")) {
      unlockBinding(as.name(i), env = ms)
      assign(i, get(i), envir = ms)
    # } else {
      # eval(substitute(environment(j) <- ms, list(j = as.name(i))))
      # # assignInNamespace(i, get(i), ms) # doesn't work...
    # }
  }
  environment(C_copyMostAttrib) <- ms
  environment(C_DUPLICATE_ATTRIB) <- ms
  # rm(list = grep("_ms", ms_replaced, value = TRUE, invert = TRUE), envir = parent.frame()) 
  attachNamespace(ms)
  rm(ms, i)
}


# funique2 <- kit::funique
