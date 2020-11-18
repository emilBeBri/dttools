#' short description 1
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
#'
#' @import data.table
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export

notdups <- function(DT, by=colnames(DT)) {
    # DT <- g1
    # by <- 'pnr'
  if(any(by %nin% colnames(DT))) stop('variable findes ikke i DT')
  dup_index <-  sort(unique(c(
  which(duplicated(DT, by=by)),
  which(duplicated(DT, by=by, fromLast=T))
  )))
  DT[!dup_index]
}
# plads til hastighedsforbedringer! #todo# 
# se efter forbedringen af dups
# alle der IKKE har dups

