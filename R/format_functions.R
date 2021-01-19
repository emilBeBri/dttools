

#' @export
#formatér for at oege laesbarheden
nrf <- function(df) {
  euformat(nrow(df))
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
  x1 <- euformat(nrow(dtx) - nrow(dty))
  x2 <- euformat(nrow(dtx))
  x3 <- euformat(nrow(dty))
  x4 <- pct(nrow(dty) / nrow(dtx))
  message(paste(x1 %+% ' rows,', x4 %+% ' --', 'nrow(df1): ' %+% x2, 'nrow(df2): ' %+% x3, sep=' '))
} 



#' @export
#formatér for at oege laesbarheden
pct <- function(df, decimals=2) {
  euformat(round(df*100,decimals) %+% ' %')
}


#' @export
#formatér for at oege laesbarheden
# tidligere hed den bare f() - det er uhensigtsmaessigt
f_pct <- function(df, decimals=2) {
  euformat(round(df*100,decimals) %+% ' %')
}


#' @export
# numre i EU-format
eu_format <- function(x) trimws(format(x, big.mark='.',decimal.mark=',', scientific=FALSE)) 
#' @export
euformat <- function(x) eu_format(x)


#' @export
# til eyeballing, samme som ovenstaaende - brug ALDRIG i bestandig kode!!
f <- function(x) euformat(x)

#' @export
# til eyeballing, samme som ovenstaaende - brug ALDRIG i bestandig kode!!
f_sum <- function(x) trimws(euformat(sum(x)))


#' @export
# numre med kr bag paa
dkk_format <- function(x) trimws(euformat(x) %+% ' kr.')

