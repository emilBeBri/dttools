#' Unique combination of two vectors
#'
#' Given two vectors will find all unique pairs.
#'
#' @param x Vector
#' @param y Vector
#' @param include.equals if FALSE will not return a pair that is the same
#' element paired against itself
#'
#' @import data.table
#'
#' @return dataframe
#' @examples
#'\dontrun{
#' x <- c('one', 'two', 'three')
#' y <- c('a', 'b', 'c')
#' function(arg1)
#' expand_grid_unique(x, y)
#'}
#'
#' 
#' @export


# from
# https://rdrr.io/github/Swarchal/TISS/src/R/similarity.R
expand_grid_unique <- function(x, y, include.equals=FALSE, as.data.table=TRUE) {
    # x <- a1$eg1
    # y <- a1$årmdr
    # include.equals <- FALSE

  x <- unique(x)
  y <- unique(y)

  g <- function(i) {
      z <- setdiff(y, x[seq_len(i-include.equals)])
      if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  out1 <- do.call(rbind, lapply(seq_along(x), g))
  if(as.data.table==TRUE) out1 <- as.data.table(out1)
  out1
}


