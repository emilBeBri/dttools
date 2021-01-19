#' short description 1
#'
#' @description descriptin 2??
#'
#' @param df a dataframe 
#'
#' @importFrom clipr write_clip
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export

# ikke så vigtig lige nu - skriv til clipboard
# #' @export
# # write to clip manager
dt_write_clip <- function(df1, var, n=10, all=FALSE, sample=TRUE) {
  # require(clipr)
  if(all==TRUE) n <- nrow(df1)

  if(sample==TRUE) {
    clipr::write_clip(
    df1[seq(1,nrow(df1),ceiling((nrow(df1)/n))), get(var)]
    , col.names=F)
  } else clipr::write_clip(df1[1:n, get(var)] , col.names=F)
}


