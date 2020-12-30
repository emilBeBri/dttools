#' insert elements in a vector at a specifik position
#'
#' @description insert elements in a vector at a specifik position. can't be in the beginning of a vector, perhaps fix sometime #todo#
#'
#' @param vector1 a vector 
#' @param pos positions of were the elements/vectors should be inserted. Length must be the same as the number of vectors inserted.  
#' @param ... vectors that should be inserted 
#'
#' @import data.table
#' @importFrom magrittr "%>%"
#'
#' @return This function creates, by reference, four variables with run- and spell-length-type information. 
#' @examples
#'\dontrun{
#' # first example
#'a1 = c(1:10)
#'b1 = c('SE MIG')
#'c1 = c('A', 'B')
#'
#'#indsaetter b efter 3. element og d efter 7. element i a 
#'insert_at(a1, c(3,7), b1, c1) 
#'#andet eksempel
#'insert_at(a1, c(4,7,9), "hello", "this", "world") 
#'
#'}
#' @export
#' 

insert_at <- function(vector1, pos, ...){
  dots <- list(...)
  stopifnot(length(dots)==length(pos))
  result <- vector("list",2*length(pos)+1)
  result[c(TRUE,FALSE)] <- split(vector1, cumsum(seq_along(vector1) %in% (pos+1)))
  result[c(FALSE,TRUE)] <- dots
  unlist(result)
}

