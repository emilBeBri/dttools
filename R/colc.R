#' search column names based on regex
#'
#' @description search column names based on a regex, possibly exclude some elements based on another regex, and include some other elements (not regex but identical). For selection of columns for use in various situations
#'
#' @param DT a character vector. if a data.frame, uses the colnames() to extract a character vector with the colnames
#' @param x a regex to search for in DT
#' @param not a regex that will exclude these from argument x. Note that this uses grepl, and since grepl is not vectorized, if several elements are supplied, they are stringed together with '|' in grepl.
#' @param plus a character vector with specific elements from the string that will be included. the function will trhow an error if these do not exist in the vector. 
#' @param ignore.case should cases be ignored? Default is TRUE
#' @import data.table 
#' @import assertthat 
#' @export
#'
#' @examples
#'\dontrun{
#' colc(dupstestdata)
#' }
#' @return This function returns a \code{character vector} that matches the regex search pattern  

colc <- function(DT, x=NA, not=NA, plus=NA, ignore.case=TRUE) {
    # DT <- c('a','b')
    # x <- 'NA'
    # not <- '6'
    # plus <- c('a','x')
    # ignore.case <- TRUE

  # hvis x (search string) er en vector (fx input er colnames), lav til "or" statement i regex, saa den soeger paa det hele
  if( length(x) > 1 ) {
    x <- paste(x, collapse='|')
    warning('search string var en vector > 1. collapser den med paste')
  }
  # er det en dataframe?
  if(is.data.frame(DT)) search_vector <- colnames(DT) else search_vector <- DT

  # errorcheck
  assertthat::assert_that(is.character(search_vector))

  if(is.na(x)) {
    x1 <- search_vector
  } else {
  x1 <- grep(x, search_vector, value=TRUE, ignore.case=ignore.case)
  }

  if(any(!is.na(not))) {
    if(length(not)==1){
    x1 <- x1[which(!grepl(not, x1, ignore.case=ignore.case))]
    } else {
      not <- paste(not, collapse='|')
      x1 <- x1[which(!grepl(not, x1, ignore.case=ignore.case))]
    }
  }

  # kun sorteres hvis det er et underudvalg af cols og ikke alle cols, for saa bliver de ændret i rækkefoelgen naar du bruger dem til at udvælge i en DT 
  if( !is.na(x)) x1 <- sort(x1) 

  if( any(!is.na(plus))) {
    # errorcheck: 'plus' needs to be in the search_vector, if not      
    if( any(plus %nin% search_vector)){
      error_out <- setdiff(plus, search_vector)
      stop('some element(s) in plus are not in the vetor: ', error_out)
    } 
    x1 <- c(plus,x1)
  }  

  
  if(any(is.na(x1))) {
    stop('der er en NA i vectoren - fejltjek funktionen')
  }    
  x1
}



