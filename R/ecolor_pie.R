#' short description 1
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
#'
#' @import data.table
#' @import ggplot2
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb2hsv
#' @importFrom graphics par
#' @importFrom graphics pie
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' ecolor_pie(ecolor_data[grp %in% 'paul tol 14', hex])
#' 
#'}
#' @export




# og som pie chart med HSV farver
ecolor_pie <- function(col) {
    colorx <- NULL # programming with data.table

	  # errorcheck
  if( !is.vector(colorx)) stop('this is not a vector')
	par(mar=c(1,1,1,1))
	ones <- rep(1, length=length(col))
	names(ones) <- apply(round(rgb2hsv(col2rgb(c(col))),2), 2,function(x) {paste(x, collapse=", ")})

	pie(ones, col=col, cex=.75, main = "HSV vaerdier")
	par(mar=c(5.1, 4.1, 4.1, 2.1))
}
# color_pie(ecolor_data[grp %in% 'xmen', hex])




