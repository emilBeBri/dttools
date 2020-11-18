#' @export
`%nilike%` <- Negate(data.table::`%ilike%`)



#' @export
 # not in
`%nin%` <- Negate(`%in%`)

#' @export
  # not like
`%nlike%` <- Negate(`%like%`)

#' @export
`%+%` <- function(x,y) paste0(x, y)
	# paste sammen ('infix')
	# OBS! ggplot2 har en funktion med samme navn. Men det er ikke sandsynligt at du nogensinde skal bruge den til noget.
	# se her:
	# https://ggplot2.tidyverse.org/reference/gg-add.html
	# taget herfra:
	# https://guillaumepressiat.github.io//blog/2019/01/stringfix


#' @export
# not chin (fra data.table)
`%nchin%` <- Negate(data.table::`%chin%`)


#' @export
`%nbetween%` <- Negate(`%between%`)


#' @export
`%agrepl%` <- function(x,y) agrepl(y, x, ignore.case=TRUE)
