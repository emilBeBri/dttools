#' find dups in different ways
#'
#' these functions adds functionality lacking in data.table and other packages that are concerned with dups
#'
#' do different stuff, yes, that's what happens!
#' @param DT a data.table with suspcted dups
#' @param by the columns by which the dups should be detected
#' @param index if TRUE, the indices are returned instead of the rows of the DT with dups.
#' @import data.table 
#' @export
#'
#'
#'
#' @examples
#' dups(dupstestdata)
#' dups(dupstestdata, index = TRUE)
#' @return This function returns a \code{data.table} with all the dups by the specified columns, instead of 'all the dups minus one' as is standard in data.table

# alle dups, ikke kun de næste-efter-den-første
dups <- function(DT, by=colnames(DT), index=FALSE) {
    # DT <- a1
    # by <- colnames(DT)
  nmax <- NULL
  if(!is.data.table(DT)) {
    data.table::setDT(DT)
    warning('DT wasn\'t a data.table - turning it into one')
  }
  dup_index <-  sort(unique(c(
  which(duplicated(DT, by=by)),
  which(duplicated(DT, by=by, fromLast=T))
  )))
  if(index==TRUE) return(dup_index)
  if(index==FALSE) {
    DT1 <- DT[dup_index]
    DT1[, nmax := 1:.N, by=c(colnames(DT1))]
    data.table::setorderv(DT1, c(colnames(DT), 'nmax'))
    return(DT1[])
  }
}




