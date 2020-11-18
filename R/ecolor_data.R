#' collections of color palettes
#'
#' A dataset containing collections of color palettes
#'
#' @format A data.table with:
#' \describe{
#'   \item{color}{color in human readable form. for now, only colornames that R understands}
#'   \item{hex}{a hex value of the color}
#'   \item{grp}{the palette the color belongs to}
#'   \item{note}{note designed to be searchable, e.g. search for "gender" and get suggestions for colors from this variable}
#'   ...
#' }
#' @source various places on the internet, and in base-R...
"ecolor_data"