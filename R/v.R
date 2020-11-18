#' this blah blah
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
#'
#' @importFrom utils View
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export

v <- function(DT, all=FALSE, n=1000, sample=FALSE) {
    # DT <- den2 
    # all=FALSE
    # n=10
    # sample=TRUE

  # if a variable in the DT is already called n, there will be a problem. so let's rename n right away
    n_number_x <- n 
    n  <- NULL

  # DT= 1:5
  if(is.data.frame(DT)==FALSE) DT <- data.frame(DT)
  if(nrow(DT) < n_number_x) n_number_x <- nrow(DT)

  if(all==FALSE & sample==FALSE) {
    utils::View(DT[1:n_number_x,])
  } else if(all==FALSE & sample==TRUE){
    DT <- DT[sample(.N, n_number_x)]
    utils::View(DT[1:n_number_x,])    
  } else utils::View(DT)
}
