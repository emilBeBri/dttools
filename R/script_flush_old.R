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


#  fjern gamle versioner af scripts
script_flush_old <- function(dir='r/', extension='r') {

    script <- family <- script <- NULL # programming with data.table


  x <- script_management_internal(dir=dir, extension=extension)

  # catches those that has a version-numbering
  x1 <- x[grepl(paste0('v[0-9]{1,}', '\\.',extension), script, ignore.case=TRUE)]


  # sidste nye script/skrig
  setorder(x1, family, -version, na.last=TRUE)
  nyeste_scripts <- x1[x1[, .I[1], .(family)]$V1]
  
  # de versioner skal smides ud: alle andre
  gamle_versioner <- x1[script %nin% nyeste_scripts$script]
  if( nrow(gamle_versioner) > 0 ) {  
    t1 <- file.copy(dir %+% gamle_versioner$script, dir %+% '00-trash', overwrite=TRUE)
    if(any(t1) == FALSE) stop('fejl i file.copy - et script blev ikke sendt til trash, tjek hvorfor')
    t2 <- file.remove(dir %+% gamle_versioner$script)
    if( any(t2) == FALSE) stop('fejl i file.remove - et script blev ikke sendt til trash, tjek hvorfor')
  }
}



