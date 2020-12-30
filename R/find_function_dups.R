#' short description 1
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
#'
#' @importFrom purrr map
#' @importFrom utils installed.packages
#' @import data.table
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export



find_function_dups <- function() {
	  # programming with data.table
    nmax <- funk <- grp <-  NULL 

	libnames <- installed.packages()[,1]
	#exclude packages: 
	libnames <- libnames[ !(libnames %in% c("rJava","RPostgreSQL","XLConnect","xlsx","xlsxjars", 'conflicted')) ]

	df <- purrr::map_dfr(libnames, ~{
	        # .x <- libnames[10]
	    com <- paste0("require(", .x,")")
	    eval(parse(text= com))
	    str <- paste0("package:", .x)

	    # all the funktions in the package
	    funk <- (ls(str))
	    if (length(funk)==0){
	      funk <- ifelse((length(funk)==0)==TRUE, "funkyFun emil", funk)
	    }

	    data.table::data.table(package=.x, funk)
	 })

	df[, nmax := .N, by='funk']
	setorder(df, funk)
	df2 <- df[nmax > 1]

	df2[, grp := .GRP, by='funk']
	df2

}






