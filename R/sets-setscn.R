
#' short description 1
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
#'
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#' function(arg1)
#' @export

sets <- function(x, y){
  # output
  list(
  in_both=intersect(x, y),
  in_x= setdiff(x, y),
  in_y= setdiff(y, x)
  )
}


#' short description 1
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
#'
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#' function(arg1)
#' @export

setscn <- function(x, y){
      # x <- kdf_tmp1
      # y <- kdf

  x <- colnames(x)
  y <- colnames(y)
  # output
  list(
  in_both=intersect(x, y),
  in_x= setdiff(x, y),
  in_y= setdiff(y, x)
  )
}


