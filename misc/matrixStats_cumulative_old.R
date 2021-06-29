colCumsums_ms <- matrixStats::colCumsums
colCumsums <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...) {
  ax <- attributes(x)
  res <- colCumsums_ms(x, rows, cols, dim., ...)
  if(length(ax) > 1L) { # more than just "dim"
    if(length(cols)) { 
      if(length(rows)) {
        dn <- dimnames(x)
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- dimnames(x)[[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- dimnames(x)[[1L]]      
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
        dn <- dimnames(x)
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- dimnames(x)[[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- dimnames(x)[[1L]]      
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
        dn <- dimnames(x)
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- dimnames(x)[[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- dimnames(x)[[1L]]      
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
        dn <- dimnames(x)
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- dimnames(x)[[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- dimnames(x)[[1L]]      
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
        dn <- dimnames(x)
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- dimnames(x)[[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- dimnames(x)[[1L]]      
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
        dn <- dimnames(x)
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- dimnames(x)[[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- dimnames(x)[[1L]]      
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
        dn <- dimnames(x)
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- dimnames(x)[[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- dimnames(x)[[1L]]      
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
        dn <- dimnames(x)
        if(length(dn)) ax[["dimnames"]] <- list(dn[[1L]][rows], dn[[2L]][cols])
      } else {
        cn <- dimnames(x)[[2L]]      
        if(length(cn)) ax[["dimnames"]][[2L]] <- cn[cols]
      }
      ax[["dim"]] <- dim(res)
    } else if(length(rows)) {
      rn <- dimnames(x)[[1L]]      
      if(length(rn)) ax[["dimnames"]][[1L]] <- rn[rows]
      ax[["dim"]] <- dim(res)
    }
    attributes(res) <- ax # faster for matrices than collapse::setAttrib
  }
  res
}
