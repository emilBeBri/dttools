
#' @export
# funktion der automatisk adjuster width paa konsollen. saettes default til hele width, men andet kan indsaettes 
wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}


