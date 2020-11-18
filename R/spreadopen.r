#' short description 1
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export

spreadopen <- function(x) {
	open_command <- switch(Sys.info()[['sysname']],
	 	Windows= 'open',
	 	Linux  = 'xdg-open',
	 	Darwin = 'open')
	return(system(paste(open_command, x)))
}


