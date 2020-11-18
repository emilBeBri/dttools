

#' find number of dups, 
#'
#' here is a long description
#'
#' not sure about this right here
#' @param DT A data.table
#' @param by the variables used to identify the dups
#' @import data.table
#' @export
#'
#' @examples
#'\dontrun{
#' dupsN(DT, by='variable1')
#' }
#' @return This function returns an \code{integer} value with the number of duplicates 
#' 
dupsN <- function(DT, by=colnames(DT), mode='all') {
    # DT <- a1$both
    # by <- colnames(DT)
    dup_tool <- NULL # programming with data.table
  
  if(mode == 'all') {    
    dup_index <-  sort(unique(c(
    which(duplicated(DT, by=by)),
    which(duplicated(DT, by=by, fromLast=T))
    )))
    return(length(dup_index)    )
  }
  if(mode == 'unique') {    
    DT1 <- DT[duplicated(DT, by=by), by, with=FALSE]
    DT2 <-  DT1[, dup_tool := 1:.N, by=by]
    return(DT2[dup_tool %in% 1, .N])
  }
}
