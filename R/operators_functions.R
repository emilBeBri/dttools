#' @export
`%nilike%` <- Negate(data.table::`%ilike%`)
`%!ilike%` <- Negate(data.table::`%ilike%`) # bedre - brug fra nu af. Den anden games for legacy purposes.
`%!flike%` <- Negate(data.table::`%flike%`)


#' @export
 # not in
`%nin%` <- Negate(`%in%`) # gemmes for legacy-purposes
`%!in%` <- Negate(`%in%`) 

#' @export
  # not like
`%nlike%` <- Negate(`%like%`) # gemmes for legacy-purposes
`%!like%` <- Negate(`%like%`) # gemmes for legacy-purposes

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
`%nchin%` <- Negate(data.table::`%chin%`) # gemmes for legacy-purposes
`%!chin%` <- Negate(data.table::`%chin%`) 


#' @export
`%nbetween%` <- Negate(data.table::`%between%`) # gemmes for legacy-purposes
`%!between%` <- Negate(data.table::`%between%`)


#' @export
`%alike%` <- function(x,y) agrepl(y, x, ignore.case=TRUE)
`%!alike%` <- Negate(`%alike%`)


