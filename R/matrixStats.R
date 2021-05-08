##########################################
# Add attribute handling capabilitis to 
# matrixStats functions where is is not
# the case.
##########################################

# These functions already handle attributes...
ms_handles_attr <- c("colWeightedMeans", "colWeightedMedians", "rowWeightedMedians",
                     "colWeightedVars", "rowWeightedVars", "colVarDiffs", "rowVarDiffs",
                     "colWeightedSds", "rowWeightedSds", "colSdDiffs", "rowSdDiffs",
                     "colWeightedMads", "rowWeightedMads", "colMadDiffs", "rowMadDiffs",
                     "colQuantiles", "rowQuantiles", "colIQRDiffs", "rowIQRDiffs")

rowWeightedMeans_ms <- matrixStats::rowWeightedMeans
rowWeightedMeans <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowWeightedMeans_ms(x, w, rows, cols, na.rm, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn # if(is.character(rows)) ckmatch(rows, rn) else -> Error for some...
  res
}

colMeans2_ms <- matrixStats::colMeans2
colMeans2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMeans2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn # if(is.character(cols)) ckmatch(cols, cn) else -> error for some
  res
}

rowMeans2_ms <- matrixStats::rowMeans2
rowMeans2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMeans2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colSums2_ms <- matrixStats::colSums2
colSums2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colSums2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn # if(is.character(cols)) ckmatch(cols, cn) else -> error for some
  res
}

rowSums2_ms <- matrixStats::rowSums2
rowSums2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowSums2_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colMedians_ms <- matrixStats::colMedians
colMedians <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMedians_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMedians_ms <- matrixStats::rowMedians
rowMedians <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMedians_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colVars_ms <- matrixStats::colVars
colVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colVars_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowVars_ms <- matrixStats::rowVars
rowVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowVars_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colSds_ms <- matrixStats::colSds
colSds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colSds_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowSds_ms <- matrixStats::rowSds
rowSds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowSds_ms(x, rows, cols, na.rm, center, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colMads_ms <- matrixStats::colMads
colMads <- function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMads_ms(x, rows, cols, center, constant, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMads_ms <- matrixStats::rowMads
rowMads <- function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMads_ms(x, rows, cols, center, constant, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colIQRs_ms <- matrixStats::colIQRs
colIQRs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colIQRs_ms(x, rows, cols, na.rm, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowIQRs_ms <- matrixStats::rowIQRs
rowIQRs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowIQRs_ms(x, rows, cols, na.rm, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colRanges_ms <- matrixStats::colRanges
colRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colRanges_ms(x, rows, cols, na.rm, dim., ...)
  dimnames(res) <- list(if(length(cols)) cn[cols] else cn, c("Min", "Max"))
  res
}

rowRanges_ms <- matrixStats::rowRanges
rowRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowRanges_ms(x, rows, cols, na.rm, dim., ...)
  dimnames(res) <- list(if(length(rows)) rn[rows] else rn, c("Min", "Max")) 
  res
}

colMins_ms <- matrixStats::colMins
colMins <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMins_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMins_ms <- matrixStats::rowMins
rowMins <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMins_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colMaxs_ms <- matrixStats::colMaxs
colMaxs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMaxs_ms(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMaxs_ms <- matrixStats::rowMaxs
rowMaxs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMaxs_ms(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAnyMissings_ms <- matrixStats::colAnyMissings
colAnyMissings <- function(x, rows = NULL, cols = NULL, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnyMissings_ms(x, rows, cols, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnyMissings_ms <- matrixStats::rowAnyMissings
rowAnyMissings <- function(x, rows = NULL, cols = NULL, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnyMissings_ms(x, rows, cols, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAnyNAs_ms <- matrixStats::colAnyNAs
colAnyNAs <- function(x, rows = NULL, cols = NULL, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnyNAs_ms(x, rows, cols, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnyNAs_ms <- matrixStats::rowAnyNAs
rowAnyNAs <- function(x, rows = NULL, cols = NULL, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnyNAs_ms(x, rows, cols, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAnys_ms <- matrixStats::colAnys
colAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnys_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnys_ms <- matrixStats::rowAnys
rowAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnys_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAlls_ms <- matrixStats::colAlls
colAlls <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAlls_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAlls_ms <- matrixStats::rowAlls
rowAlls <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAlls_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colCounts_ms <- matrixStats::colCounts
colCounts <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colCounts_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowCounts_ms <- matrixStats::rowCounts
rowCounts <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowCounts_ms(x, rows, cols, value, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colCumsums_ms <- matrixStats::colCumsums
colCumsums <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- colCumsums_ms(x, rows, cols, dim., ...)
  if(length(ax) > 1L) { # more than just "dim"
    if(length(cols)) { 
      if(length(rows)) {
        dn <- ax[["dimnames"]]
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- ax[["dimnames"]][[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- ax[["dimnames"]][[1L]]      
      if(length(rn)) ax[["dimnames"]][[1L]] <- rn[rows]
      ax[["dim"]] <- dim(res)
    }
    attributes(res) <- ax # faster for matrices than collapse::setAttrib
  }
  res
}

rowCumsums_ms <- matrixStats::rowCumsums
rowCumsums <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- rowCumsums_ms(x, rows, cols, dim., ...)
  if(length(ax) > 1L) { # more than just "dim"
    if(length(cols)) { 
      if(length(rows)) {
        dn <- ax[["dimnames"]]
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- ax[["dimnames"]][[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- ax[["dimnames"]][[1L]]      
      if(length(rn)) ax[["dimnames"]][[1L]] <- rn[rows]
      ax[["dim"]] <- dim(res)
    }
    attributes(res) <- ax # faster for matrices than collapse::setAttrib
  }
  res
}

colCumprods_ms <- matrixStats::colCumprods
colCumprods <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- colCumprods_ms(x, rows, cols, dim., ...)
  if(length(ax) > 1L) { # more than just "dim"
    if(length(cols)) { 
      if(length(rows)) {
        dn <- ax[["dimnames"]]
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- ax[["dimnames"]][[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- ax[["dimnames"]][[1L]]      
      if(length(rn)) ax[["dimnames"]][[1L]] <- rn[rows]
      ax[["dim"]] <- dim(res)
    }
    attributes(res) <- ax # faster for matrices than collapse::setAttrib
  }
  res
}

rowCumprods_ms <- matrixStats::rowCumprods
rowCumprods <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- rowCumprods_ms(x, rows, cols, dim., ...)
  if(length(ax) > 1L) { # more than just "dim"
    if(length(cols)) { 
      if(length(rows)) {
        dn <- ax[["dimnames"]]
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- ax[["dimnames"]][[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- ax[["dimnames"]][[1L]]      
      if(length(rn)) ax[["dimnames"]][[1L]] <- rn[rows]
      ax[["dim"]] <- dim(res)
    }
    attributes(res) <- ax # faster for matrices than collapse::setAttrib
  }
  res
}

colCummins_ms <- matrixStats::colCummins
colCummins <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- colCummins_ms(x, rows, cols, dim., ...)
  if(length(ax) > 1L) { # more than just "dim"
    if(length(cols)) { 
      if(length(rows)) {
        dn <- ax[["dimnames"]]
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- ax[["dimnames"]][[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- ax[["dimnames"]][[1L]]      
      if(length(rn)) ax[["dimnames"]][[1L]] <- rn[rows]
      ax[["dim"]] <- dim(res)
    }
    attributes(res) <- ax # faster for matrices than collapse::setAttrib
  }
  res
}

rowCummins_ms <- matrixStats::rowCummins
rowCummins <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- rowCummins_ms(x, rows, cols, dim., ...)
  if(length(ax) > 1L) { # more than just "dim"
    if(length(cols)) { 
      if(length(rows)) {
        dn <- ax[["dimnames"]]
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- ax[["dimnames"]][[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- ax[["dimnames"]][[1L]]      
      if(length(rn)) ax[["dimnames"]][[1L]] <- rn[rows]
      ax[["dim"]] <- dim(res)
    }
    attributes(res) <- ax # faster for matrices than collapse::setAttrib
  }
  res
}

colCummaxs_ms <- matrixStats::colCummaxs
colCummaxs <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- colCummaxs_ms(x, rows, cols, dim., ...)
  if(length(ax) > 1L) { # more than just "dim"
    if(length(cols)) { 
      if(length(rows)) {
        dn <- ax[["dimnames"]]
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- ax[["dimnames"]][[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- ax[["dimnames"]][[1L]]      
      if(length(rn)) ax[["dimnames"]][[1L]] <- rn[rows]
      ax[["dim"]] <- dim(res)
    }
    attributes(res) <- ax # faster for matrices than collapse::setAttrib
  }
  res
}

rowCummaxs_ms <- matrixStats::rowCummaxs
rowCummaxs <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- rowCummaxs_ms(x, rows, cols, dim., ...)
  if(length(ax) > 1L) { # more than just "dim"
    if(length(cols)) { 
      if(length(rows)) {
        dn <- ax[["dimnames"]]
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- ax[["dimnames"]][[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- ax[["dimnames"]][[1L]]      
      if(length(rn)) ax[["dimnames"]][[1L]] <- rn[rows]
      ax[["dim"]] <- dim(res)
    }
    attributes(res) <- ax # faster for matrices than collapse::setAttrib
  }
  res
}

colDiffs_ms <- matrixStats::colDiffs
colDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colDiffs_ms(x, rows, cols, lag, differences, dim., ...)
  if(length(cn)) dimnames(res) <- list(NULL, if(length(cols)) cn[cols] else cn)
  res
}

rowDiffs_ms <- matrixStats::rowDiffs
rowDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowDiffs_ms(x, rows, cols, lag, differences, dim., ...)
  if(length(rn)) dimnames(res) <- list(if(length(rows)) rn[rows] else rn, NULL)
  res
}
