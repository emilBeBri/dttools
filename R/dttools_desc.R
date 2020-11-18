#' short description 1
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
#'
#' @import data.table
#' @import stats
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export



# hele DT (maaske tilfoeje dttools_catn som list-variable her?) #todo#
dttools_desc <- function(dt) {
    # dt <- copy(sru1)
    pct_na <-  NULL # programming with data.table

  cols <- colnames(dt)
  class_types <-  c('integer', 'integer64', 'double', 'numeric', 'logical', 'character', 'float64', 'factor')
  var_class <- data.table(class=sapply(dt, class), var=cols)
  var_class[, rank := mapvalues(class, from=class_types, to=1:length(class_types), warn_missing=FALSE)]

  # alle variable 
  valid_N <- data.table(var=cols, valid_N= sapply(cols, function(x) dt[!is.na(get(x)), .N]))
  N_na <- data.table(var=cols, N_na= sapply(cols, function(x) dt[is.na(get(x)), .N]))
  unique_vals <- data.table(var=cols, unique_vals= sapply(cols, function(x) dt[, length(unique(get(x)))]))

  # numeriske
  numcols <- var_class[class %in% c('integer', 'double', 'numeric', 'integer64'), var]
  # af en eller anden grund vil den have 
  numdt <- Reduce(cbind, list(data.table(var=numcols),
    dt[, .(mean=lapply(.SD, function(x) mean(x, na.rm=TRUE) )), .SDcols = numcols],
    dt[, .(sd=lapply(.SD, function(x) sd(x, na.rm=TRUE))), .SDcols = numcols],
    dt[, .(min=lapply(.SD, function(x) min(x, na.rm=TRUE))), .SDcols = numcols],
    dt[, .(max=lapply(.SD, function(x) max(x, na.rm=TRUE))), .SDcols = numcols],  
    dt[, .(p50=lapply(.SD, function(x) median(x, na.rm=TRUE))), .SDcols = numcols]  
    ))
  numdt[, (colnames(numdt)[-1]) := lapply(.SD, as.numeric), .SDcols = colnames(numdt)[-1]]

  outputdt <- Reduce(function(x,y) merge(x,y,by=c('var'), all=TRUE), list(
    var_class, valid_N, N_na, unique_vals, numdt))

  # 
  outputdt[, pct_na := pct((N_na / (valid_N +  N_na)))]
  setcolorder(outputdt, c('var', 'class', 'valid_N', 'N_na', 'pct_na'))

  # hvis der sler ikke er nogen numeric cols, saa fjern men, sd, min, max etc
  if( !any(var_class$class=='numeric')) {
		cols <- c('mean','sd','min','max','p50')
	  outputdt[, (cols) := NULL]
  }

  outputdt[order(rank), !'rank']
}
