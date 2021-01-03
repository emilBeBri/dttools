
# not ilike (from data-table)
#' @export
`%!ilike%` <- Negate(data.table::`%ilike%`) # better- use from now on. The old one kept for legacy purposes.
#' @export
`%nilike%` <- Negate(data.table::`%ilike%`) # kept for legacy purposes


# not flike (from data-table)
#' @export
`%!flike%` <- Negate(data.table::`%flike%`)

 # not-like
#' @export
`%!like%` <- Negate(data.table::`%like%`) 
#' @export
`%nlike%` <- Negate(data.table::`%like%`) # kept for legacy purposes



 # not in
#' @export
`%!in%` <- Negate(`%in%`) 
#' @export
`%nin%` <- Negate(`%in%`) # kept for legacy purposes


#' @export
`%+%` <- function(x,y) paste0(x, y)
	# paste sammen ('infix')
	# OBS! ggplot2 har en funktion med samme navn. Men det er ikke sandsynligt at du nogensinde skal bruge den til noget.
	# se her:
	# https://ggplot2.tidyverse.org/reference/gg-add.html
	# taget herfra:
	# https://guillaumepressiat.github.io//blog/2019/01/stringfix


# not chin (from data.table)
#' @export
`%!chin%` <- Negate(data.table::`%chin%`) 
#' @export
`%nchin%` <- Negate(data.table::`%chin%`) # kept for legacy purposes

# not-between
#' @export
`%!between%` <- Negate(data.table::`%between%`)
#' @export
`%nbetween%` <- Negate(data.table::`%between%`) # kept for legacy purposes


# fuzzy like and fuzzy not-like
#' @export
`%alike%` <- function(x,y) agrepl(y, x, ignore.case=TRUE)
#' @export
`%!alike%` <- Negate(`%alike%`)


