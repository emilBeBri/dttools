#' short description 1
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
#'
#' @importFrom purrr map
#' @import data.table
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export




# function til at taelle antal i kategorier
dttools_catn <- function(dt, varliste) {
    # dt <- copy(ae1)oe
    # varliste <- c('howmanyfirms')
  map(varliste, ~ {
      # .x <- varliste
    dt_out <- dt[, .N, by=.x] [, `:=` (pct = round(100*(N/sum(N)),1), and = round(N/sum(N), 4))]
    dt_out[order(get(.x))]
   })
}
