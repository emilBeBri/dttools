#' selects newest version of an R script
#'
#' @description Selects the newest function of an R script, and sources it (default). Makes sourcing a script easier to read than source() in itself, when using some form of scriptnaming where the project-name is also included in the script. Throws an error if several versions of the same scripts exists. Requires a naming convention with 
#'
#' @param dir directory to search in 
#' @param extension extension to look for
#' @param path should the path be returned or just the script name?  
#' @param source should the script be sourced? defaults to TRUE
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

script_newest <- function(script, dir='r/', extension='r', path=TRUE, source=TRUE) {  
    # dir <- 'r/'
    # extension <- 'r'
    # script <- 'scriptname'
    # path <- TRUE
 
  x <- script_management_internal(dir=dir, extension=extension)

  x1 <- x[grepl(get('script', envir = parent.env(environment())), family, ignore.case=TRUE)]



  if( nrow(x1) == 0) stop('No script was found')
  if( uniqueN(x1$family) > 1 ) stop('there are two different scripts that matches the regex:', ' \n', paste(x1$family, collapse=' \n'))
  if( nrow(x1) > 1 & uniqueN(x1$family) == 1) warning('There are ', nrow(x1), ' versions of the same script. Make sure to flush old versions with scripts_flush_old()')

  setorder(x1, family, -version, na.last=TRUE)
  x2 <- x1[x1[, .I[1], .(family)]$V1]

  # return the path as well or only the script name?
  if(path==TRUE) x3 <- dir %+% x2$script
  if(path==FALSE) x3 <- x2$script 

  # source the script or not?
  if(source==TRUE) source(x3)
  if(source==FALSE) return(x3)

}



