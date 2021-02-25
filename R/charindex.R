#' Index numbers for all letters in a string 
#'
#' @description index numbers for all letters in a string
#'
#' @param char a character vector  
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' charindex(c('Jens Peter Hanson', 'dato 2020-01-01'))
#'}
#' @export

# character index function 
charindex <- function (char) {
  lapply(char, FUN = function (x) {
    spl <- strsplit(x, "")[[1]]
    ret <- 1:length(spl)
    names(ret) <- spl
    ret
  })
}


