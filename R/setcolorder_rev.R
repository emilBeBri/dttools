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


# omarranger kolonnner bagfra
setcolorder_rev <- function(DT,cols){
  # cols <- c('mor_id_bak', 'far_id_bak')
  resten_af_cols <- setdiff(colnames(DT),cols)  
  setcolorder(DT, c(resten_af_cols, cols))
}

