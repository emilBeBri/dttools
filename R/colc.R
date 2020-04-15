#' regex a vector or columnnames of data.frame and return only the matches with the regex
#'
#' regex a vector or columnnames of data.frame and return only the matches with the regex
#'
#' regex a vector or columnnames of data.frame and return only the matches with the regex
#' @param DT a character vector. if a data.frame, uses the colnames
#' @param x a regex to search for in DT
#' @param not a regex that will exclude these from argument x 
#' @param plus a charactervector with specific names that will be included in the output 
#' @param ignore.case should cases be ignored? Default is TRUE
#' @import data.table 
#' @import assertthat 
#' @export
#'
#'
#'
#' @examples
#' colc(dupstestdata)
#' @return This function returns a \code{character vector} that matches the regex search pattern  

colc <- function(DT, x=NA, not=NA, plus=NA, ignore.case=TRUE) {
    # DT <- liste_gruppeniveauer
    # # DT <- dat1[1:10]
    # x <- NA
    # not <- '6'
    # plus <- NA
    # ignore.case <- TRUE

  # hvis x (search string) er en vector (fx input er colnames), lav til "or" statement i regex, så den søger på det hele
  if( length(x) > 1 ) {
    x <- paste(x, collapse='|')
    warning('search string var en vector > 1. collapser den med paste')
  }
  # er det en dataframe?
  if(is.data.frame(DT)) search_vector <- colnames(DT) else search_vector <- DT

  # fejlcheck
  assertthat::assert_that(is.character(search_vector))

  if(is.na(x)) {
    x1 <- search_vector
  } else {
  x1 <- grep(x, search_vector, value=TRUE,ignore.case=ignore.case)
  }

  if(any(!is.na(not))) {
    if(length(not)==1){
    x1 <- x1[which(!grepl(not, x1))]
    } else {
      not <- paste(not, collapse='|')
      x1 <- x1[which(!grepl(not, x1))]
    }
  }

  # kun sorteres hvis det er et underudvalg af cols og ikke alle cols, for så bliver de ændret i rækkefølgen når du bruger dem til at udvælge i en DT 
  if(!is.na(x)) x1 <- sort(x1) 

  if(any(!is.na(plus))) {
    x1 <- c(plus,x1)
  }  

  
  if(any(is.na(x1))) {
    stop('der er en NA i vectoren - fejltjek funktionen')
  }    
  x1
}


