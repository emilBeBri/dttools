
#' read all excel sheets
#' @description read all excel sheets
#' 
#' @param filename location of the excel file
#' @param na.strings case sensitive strings that will be coverted to NA. The function will do a trimws(x,'both') before conversion. If NULL, do only trimws, no conversion to NA.
#' @import data.table 
#' @import readxl 
#' 
#' @return Returns a vector trimws (always for factor, character) and NA converted (if matching na.strings). Attributes will also be kept ('label','labels', 'value.labels').
#' @export
#' 


read_excel_allsheets <- function(filename, tibble = FALSE) {
    # require(readxl)    
    # I prefer straight data.frames
    # but if you like tidyverse tibbles (the default with read_excel)
    # then just pass tibble = TRUE
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) data.table::as.data.table(readxl::read_excel(filename, sheet = X)))
    names(x) <- sheets
    x
}
