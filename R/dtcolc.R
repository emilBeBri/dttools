#' short description 1
#'
#' @description descriptin 2??
#'
#'
#' @import data.table
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export



dtcolc <- function(dt, ...) {
    thecols <- colc(dt, ...)
    dt[, thecols, with = FALSE]
}

