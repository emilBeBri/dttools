#' clean colnames
#'
#' @description clean colnames, janitor style but by reference
#'
#' @param x a data.table with dirty names on it
#'
#' @importFrom janitor clean_names
#' @import data.table
#'
#' @return This function returns \code{a data.table} with clean names. all arguments passed to janitor::clean_names().
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export



setnames_clean <- function(x, ...) {
		# x <- copy(regoversigt2)	
	# but with danish letters, first change into aa, oe og ae (because janitor )
	setnames(x, colnames(x), gsub('ø', 'oe', colnames(x)))
	setnames(x, colnames(x), gsub('æ', 'ae', colnames(x)))
	setnames(x, colnames(x), gsub('å', 'aa', colnames(x)))
	# using the ol' janitors magic without any of the duplication
	newnames <- colnames(janitor::clean_names(x[0], ...))
	setnames(x, colnames(x), newnames)

}



