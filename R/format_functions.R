


#' @export
#formatér for at oege laesbarheden
nrf <- function(df) {
  format(nrow(df), big.mark='.', decimal.mark=',')
}


#' nrows for one data.frame minus the nrows of another data.frame, with readable formatting
#'
#' that's about it
#'
#' not sure about this right here
#' @param dtx A data.frame
#' @param dty A data.frame
#' @export
#'
#' @return This function returns an \code{character} vector with the difference in number of rows between the two data.frames (dtx-dty), with nicely readable formatting  
#' 
#' @examples
#'\dontrun{
#' nr2(dtx, dtx)
#'}
#' 

nrf2 <- function(dtx, dty) {
    # dtx <- dup1
    # dty <- dup2
  x1 <- format(nrow(dtx) - nrow(dty), big.mark='.', decimal.mark=',')
  x2 <- format(nrow(dtx), big.mark='.', decimal.mark=',')
  x3 <- format(nrow(dty), big.mark='.', decimal.mark=',')
  x4 <- pct(nrow(dty) / nrow(dtx))
  message(paste(x1 %+% ' rows,', x4 %+% ' --', 'nrow(df1): ' %+% x2, 'nrow(df2): ' %+% x3, sep=' '))
} 



#' @export
#formatér for at oege laesbarheden
pct <- function(df, decimals=2) {
  format(round(df*100,decimals) %+% ' %',big.mark='.', decimal.mark=',')
}

#' @export
#formatér for at oege laesbarheden
# tidligere hed den bare f() - det er uhensigtsmaessigt
f_pct <- function(df, decimals=2) {
  format(round(df*100,decimals) %+% ' %',big.mark='.', decimal.mark=',')
}


#' @export
# numre i EU-format
eu_format <- function(x) trimws(format(x, big.mark='.',decimal.mark=',', scientific=FALSE))

#' @export
# til eyeballing, samme som ovenstaaende - brug ALDRIG i bestandig kode!!
f <- function(x) trimws(format(x, big.mark='.',decimal.mark=',', scientific=FALSE))

#' @export
# til eyeballing, samme som ovenstaaende - brug ALDRIG i bestandig kode!!
f_sum <- function(x) trimws(format(sum(x), big.mark='.',decimal.mark=',', scientific=FALSE))


#' @export
# numre med kr bag paa
dkk_format <- function(x) trimws(format(x, big.mark='.',decimal.mark=',', scientific=FALSE) %+% ' kr.')

