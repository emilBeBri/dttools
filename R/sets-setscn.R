#' list of all the possible sets between two vectors
#'
#' @description list of all the possible sets between two vectors
#'
#' @param x the first vector 
#' @param y the second vector 
#'
#' @return This function returns a \code{list} with the three types of sets: the \code{intersect} between two vectors, and the sets exclusive for the one or the other vector.
#' @examples
#'\dontrun{
#' sets(arg1)
#' }
#' @export

# #todo# how does this fare with large data.frames? Could you use the f-data.table functions instead
sets <- function(x, y){
  # output
  list(
  in_both=intersect(x, y),
  in_x= setdiff(x, y),
  in_y= setdiff(y, x)
  )
}


#' list of all the possible sets between two vectors of colnames
#'
#' @description list of all the possible sets between two vectors
#'
#' @param x the first data.frame 
#' @param y the second data.frame
#'
#' @return This function returns a \code{list} with the three types of sets: the \code{intersect} between two vectors of colnames, and the sets exclusive for the one or the other vector of colnames.
#' @examples
#'\dontrun{
#' sets(arg1)
#' }
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


