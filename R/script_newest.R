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


script_newest <- function(scriptname, dir='r/', extension='r', path=TRUE, source=TRUE) {  
    # dir <- 'r/'
    # extension <- 'r'
    # scriptname='custom-functions'
  x <- script_management_internal(dir=dir, extension=extension)
  x1 <- x[grepl(scriptname, family, ignore.case=TRUE)]

  if( nrow(x1) > 1) stop('There are at least two versions of the same script. Make sure to flush old versions with scripts_flush_old()')
  if( nrow(x1) == 0) stop('No script was found')


  setorder(x1, family, -version, na.last=TRUE)
  x2 <- x1[x1[, .I[1], .(family)]$V1]

  # return the path as well or only the script name?
  if(path==TRUE) x3 <- dir %+% x2$script
  if(path==FALSE) x3 <- x2$script 

  # source the script or not?
  if(source==TRUE) source(x3)
  if(source==FALSE) return(x3)

}
