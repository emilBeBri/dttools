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



dttools_na <- function(dt, var1, by=NA, numeric=FALSE) {
    # var1 <- 'n_aar'
    # dt <- sru_original
    # by <- NA
    notNA <- isNA <- andel_notNA <- NULL # programming with data.table


  # if 'by' is supplied or not
  if(!is.na(by)) {
  x1 <- merge(
  dt[, .(all=.N) , by=by],
  dt[!is.na(get(var1)), .(notNA=.N) , by=by], 
  by=by, all.x=TRUE)
  x1 <- merge(x1, dt[is.na(get(var1)), .(isNA=.N) , by=by], by=by, all.x=TRUE)
  } else {  
  x1 <- data.table(
    dt[, .(all=.N)],
    dt[!is.na(get(var1)), .(notNA=.N)],
    dt[is.na(get(var1)), .(isNA=.N)]
    )
  }
  # formattering
  x1[is.na(notNA) , notNA := 0]
  x1[is.na(isNA) , isNA := 0]
  x1[!is.na(notNA), andel_notNA := f_pct(notNA/all)]
  cols <- c('all', 'notNA', 'isNA')
  x1[, (cols) := lapply(.SD, f), .SDcols = cols]
  copy(x1)
}

# du kan ikke have en funktion der hedder "f". det skal laves om. #todo#



