#' sample from panel or longitutional data, where you'd like all the observations from a unique id (that is sampled)
#'
#' not sure about this right here
#' @param datx data.table or data.frame
#' @param idvar variable that identifies a unit across observations
#' @param n number of units (*not* number of observations)
#' @import data.table
#' @export
#'
#' @examples
#'\dontrun{
#' function(arg1, arg2 etc)
#' }
#' @return This function returns a \code{data.table} with samples from a larger data.table
#' 
sample_long <- function(datx, idvar, n=5) {
    # datx <- sru_original
    # idvar <- 'person_id'
    # n <- 10
  # x1 <- datx[, .SD[sample(.N, min(.N, n))]][[idvar]]
  # datx[person_id %in% x1]
  x1 <- eval(substitute(datx[, .SD[sample(.N, min(.N, n))]]$idvar))
  eval(substitute(datx[idvar %in% x1]))
}

