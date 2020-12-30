#' Internal function for memory_usage
#'
#' @description Internal function for memory_usage
#'
#' @import data.table
#' @importFrom utils object.size
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}

# from this question. function made by Dirk E. 
# https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
.ls.objects <- function (pos = 1, pattern, n=10, all=FALSE) {
		# pos <- 1
		# pattern <- ''

	napply <- function(names, fn) sapply(names, function(x)
	    fn(get(x, pos = pos)))

	names <- ls(pos = pos, pattern = pattern)
	
	obj.class <- napply(names, function(x) as.character(class(x))[1])
	obj.mode <- napply(names, mode)
	obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
	obj.prettysize <- napply(names, function(x) {
	    format(utils::object.size(x),  units = "auto") })
	obj.size <- napply(names, utils::object.size)

	obj.dim <- t(napply(names, function(x)
	    as.numeric(dim(x))[1:2]))
	vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
	obj.dim[vec, 1] <- napply(names, length)[vec]
	out <- data.table(obj.type, obj.size, obj.prettysize, obj.dim, keep.rownames=TRUE)
	setnames(out, c("type", "size", "prettysize", 'name', "rows", "columns"))
	setcolorder(out, 'name')
	setorderv(out, 'size', order=-1)

  # data
  if(n > nrow(out)) n <- nrow(out)

  if(all == TRUE) out <- out
  if(all == FALSE) out <-out[1:n]

	out # output

}

#' short description 1
#'
#' @description descriptin 2??
#'
#' @param n top n - default is the 10 most memory consuming 
#' @param all should all the objects be returned? Default is FALSE.  
#'
#' @return This function returns a \code{data.table} with memory usage for the objects in the R session
##' @examples
#'\dontrun{
#' memory_usage(n=15)
#' memory_usage(all=TRUE)
#'}
#' @export

# shorthand
memory_usage <- function(..., n=10) {
	.ls.objects(..., n=n)
}









