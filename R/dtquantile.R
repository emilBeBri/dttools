#' short description 1
#'
#' @description descriptin 2??
#'
#' @param x a vector 
#'
#' @importFrom scales percent
#' @importFrom stats quantile
#' @import data.table
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export


# quantiler som data.table 
dtquantile <- function(x,start=0,slut=1,interval=0.05,char=TRUE, na.rm=TRUE) {
    # x <- sru1$name_ssh
    # start <- 0
    # slut <- 1
    # interval <- 0.05
    # char <- TRUE
    # na.rm <- TRUE
    value <- value_chr <- NULL # programming with data.table

  seq_vector <- seq(start,slut,interval)
    dtx <- data.table(
      value=quantile(x,seq_vector,na.rm=na.rm), 
      p_char=scales::percent(seq_vector), 
      p=seq_vector*100
    )
    if( char==TRUE){
      dtx[, value_chr := eu_format(value)]
      setcolorder(dtx, 'value_chr')
      return(copy(dtx)) # output
    } else(return(dtx))
}

