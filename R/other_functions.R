
#' @export
# funktion der automatisk adjuster width. sættes default til hele width, men andet kan indsættes 
wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}


