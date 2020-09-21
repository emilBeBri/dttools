


#' @export
#formatér for at øge læsbarheden
nrf <- function(df) {
  format(nrow(df), big.mark='.', decimal.mark=',')
}


#' @export
#formatér for at øge læsbarheden
pct <- function(df, decimals=2) {
  format(round(df*100,decimals) %+% ' %',big.mark='.', decimal.mark=',')
}

#' @export
#formatér for at øge læsbarheden
# tidligere hed den bare f() - det er uhensigtsmæssigt
f_pct <- function(df, decimals=2) {
  format(round(df*100,decimals) %+% ' %',big.mark='.', decimal.mark=',')
}


#' @export
# numre i EU-format
eu_format <- function(x) trimws(format(x, big.mark='.',decimal.mark=',', scientific=FALSE))

#' @export
# til eyeballing, samme som ovenstående - brug ALDRIG i bestandig kode!!
f <- function(x) trimws(format(x, big.mark='.',decimal.mark=',', scientific=FALSE))

#' @export
# til eyeballing, samme som ovenstående - brug ALDRIG i bestandig kode!!
f_sum <- function(x) trimws(format(sum(x), big.mark='.',decimal.mark=',', scientific=FALSE))


#' @export
# numre med kr bag på
dkk_format <- function(x) trimws(format(x, big.mark='.',decimal.mark=',', scientific=FALSE) %+% ' kr.')

