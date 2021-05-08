# MatrixStats

m <- collapse::qM(mtcars)
X <- tsbox::ts_xts(EuStockMarkets)
v <- m[, 1]

library(matrixStats)

colWeightedMeans(m, w = v, cols = 1:3) # Working names!!

rowWeightedMeans(m, w = m[1, ], rows = 1:3)
rowWeightedMeans. <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowWeightedMeans(x, w, rows, cols, na.rm, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn # if(is.character(rows)) ckmatch(rows, rn) else -> Error for some...
  res
}

colMeans2(m, cols = 1:3)
colMeans2. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMeans2(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn # if(is.character(cols)) ckmatch(cols, cn) else -> error for some
  res
}

rowMeans2(m, rows = 1:3)
rowMeans2. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMeans2(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colSums2(m, cols = 1:3)
colSums2. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colSums2(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn # if(is.character(cols)) ckmatch(cols, cn) else -> error for some
  res
}

rowSums2(m, rows = 1:3)
rowSums2. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowSums2(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colMedians(m, cols = 1:3)
colMedians. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMedians(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMedians(m, rows = 1:3)
rowMedians. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMedians(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colWeightedMedians(m, v, cols = 1:3)      # Keeps attributes...
rowWeightedMedians(m, m[1, ], rows = 1:3) # Keeps attributes...

colVars(m, cols = 1:3)
colVars. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colVars(x, rows, cols, na.rm, center, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowVars(m, rows = 1:3)
rowVars. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowVars(x, rows, cols, na.rm, center, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colWeightedVars(m, v, cols = 1:3)      # Keeps attributes...
rowWeightedVars(m, m[1, ], rows = 1:3) # Keeps attributes...
colVarDiffs(m, cols = 1:3)             # Keeps attributes...  
rowVarDiffs(m, rows = 1:3)             # Keeps attributes...   

colSds(m, cols = 1:3)
colSds. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colSds(x, rows, cols, na.rm, center, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowSds(m, rows = 1:3)
rowSds. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowSds(x, rows, cols, na.rm, center, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colWeightedSds(m, v, cols = 1:3)      # Keeps attributes...
rowWeightedSds(m, m[1, ], rows = 1:3) # Keeps attributes...
colSdDiffs(m, cols = 1:3)             # Keeps attributes...  
rowSdDiffs(m, rows = 1:3)             # Keeps attributes...   

colMads(m, cols = 1:3)
colMads. <- function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMads(x, rows, cols, center, constant, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMads(m, rows = 1:3)
rowMads. <- function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMads(x, rows, cols, center, constant, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colWeightedMads(m, v, cols = 1:3)      # Keeps attributes...
rowWeightedMads(m, m[1, ], rows = 1:3) # Keeps attributes...
colMadDiffs(m, cols = 1:3)             # Keeps attributes...  
rowMadDiffs(m, rows = 1:3)             # Keeps attributes...   
colQuantiles(m, cols = 1:3)            # Keeps attributes...  
rowQuantiles(m, rows = 1:3)            # Keeps attributes...   


colIQRs(m, cols = 1:3)
colIQRs. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colIQRs(x, rows, cols, na.rm, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowIQRs(m, rows = 1:3)
rowIQRs. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowIQRs(x, rows, cols, na.rm, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colIQRDiffs(m, cols = 1:3)             # Keeps attributes...  
rowIQRDiffs(m, rows = 1:3)             # Keeps attributes...   

colRanges(m, cols = 1:3)             
colRanges. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colRanges(x, rows, cols, na.rm, dim., ...)
  dimnames(res) <- list(if(length(cols)) cn[cols] else cn, c("Min", "Max"))
  res
}

rowRanges(m, rows = 1:3)
rowRanges. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowRanges(x, rows, cols, na.rm, dim., ...)
  dimnames(res) <- list(if(length(rows)) rn[rows] else rn, c("Min", "Max")) 
  res
}

colMins(m, cols = 1:3)
colMins. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMins(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMins(m, rows = 1:3)
rowMins. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMins(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colMaxs(m, cols = 1:3)
colMaxs. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colMaxs(x, rows, cols, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowMaxs(m, rows = 1:3)
rowMaxs. <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowMaxs(x, rows, cols, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}


colAnyMissings(m, cols = 1:3)
colAnyMissings. <- function(x, rows = NULL, cols = NULL, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnyMissings(x, rows, cols, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnyMissings(m, rows = 1:3)
rowAnyMissings. <- function(x, rows = NULL, cols = NULL, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnyMissings(x, rows, cols, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAnyNAs(m, cols = 1:3)
colAnyNAs. <- function(x, rows = NULL, cols = NULL, ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnyNAs(x, rows, cols, ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnyNAs(m, rows = 1:3)
rowAnyNAs. <- function(x, rows = NULL, cols = NULL, ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnyNAs(x, rows, cols, ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAnys(m, cols = 1:3)
colAnys. <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAnys(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAnys(m, rows = 1:3)
rowAnys. <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAnys(x, rows, cols, value, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colAlls(m, cols = 1:3)
colAlls. <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colAlls(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowAlls(m, rows = 1:3)
rowAlls. <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowAlls(x, rows, cols, value, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}

colCounts(m, cols = 1:3)
colCounts. <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colCounts(x, rows, cols, value, na.rm, dim., ...)
  if(length(cn)) names(res) <- if(length(cols)) cn[cols] else cn 
  res
}

rowCounts(m, rows = 1:3)
rowCounts. <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowCounts(x, rows, cols, value, na.rm, dim., ...)
  if(length(rn)) names(res) <- if(length(rows)) rn[rows] else rn 
  res
}


colCumsums(m, cols = 1:3)
colCumsums. <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- colCumsums(x, rows, cols, dim., ...)
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

rowCumsums.(m, rows = 1:3)
rowCumsums. <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- rowCumsums(x, rows, cols, dim., ...)
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


colCumprods(m, cols = 1:3)
colCumprods. <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- colCumprods(x, rows, cols, dim., ...)
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

rowCumprods(m, rows = 1:3)
rowCumprods. <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- rowCumprods(x, rows, cols, dim., ...)
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


colCummins(m, cols = 1:3)
colCummins. <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- colCummins(x, rows, cols, dim., ...)
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

rowCummins(m, rows = 1:3)
rowCummins. <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- rowCummins(x, rows, cols, dim., ...)
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


colCummaxs(m, cols = 1:3)
colCummaxs. <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- colCummaxs(x, rows, cols, dim., ...)
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

rowCummaxs(m, rows = 1:3)
rowCummaxs. <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- rowCummaxs(x, rows, cols, dim., ...)
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

colDiffs(m, cols = 1:3)
colDiffs. <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ...) {
  cn <- dimnames(x)[[2L]]
  res <- colDiffs(x, rows, cols, lag, differences, dim., ...)
  if(length(cn)) dimnames(res) <- list(NULL, if(length(cols)) cn[cols] else cn)
  res
}

rowDiffs(m, rows = 1:3)
rowDiffs. <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ...) {
  rn <- dimnames(x)[[1L]]
  res <- rowDiffs(x, rows, cols, lag, differences, dim., ...)
  if(length(rn)) dimnames(res) <- list(if(length(rows)) rn[rows] else rn, NULL)
  res
}
