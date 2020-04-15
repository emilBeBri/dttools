#' opens data in spreadsheet program
#'
#' opens data in spreadsheet program
#'
#' opens data in spreadsheet program
#' @param data a data.frame
#' @param filter should there be the autofilter function enabled in the spreadsheet?
#' @param table as a table?
#' @param n number of rows
#' @param all should all rows be used? if TRUE, overrules the n argument.
#' @param sample should the n rows be sampled of taken from the top down?
#' @import data.table 
#' @import readxl
#' @import openxlsx
#' @export
#'
#'
#'
#'@examples
#'\dontrun{
#' dat2 <- vi(dupstestdata)
#'}
#' @return This function opens a \code{data.frame} in a spreadsheet viewer. if asssigned to an object, the data.frame as well as manual changes in the spreadsheet will be saved to that object. 


# se i excel ark
v2 <- function(data, filter=TRUE, table=TRUE, n=9000, all=FALSE, sample=FALSE) {
  #til test
  # filter <- TRUE
  # table <- TRUE
  # data <- t1
  # all <- FALSE
  # sample <- FALSE
  # n <- 9000

  # hvis vektor
  if(is.vector(data)==TRUE & is.data.frame(data)==FALSE) {
  data <- data.table::data.table(data)
  }  

  data <- data.table::as.data.table(data) #for en sikkerhedsskyld - tibbles failer fx. uden det her

  if(all==TRUE & sample==TRUE) stop('du kan ikke *baade* sample *og* tage alle med')
  if(all == FALSE & nrow(data) > n){
    mister <-  nrow(data) - n
    warning('der er flere rows end n er sat til - hvis du gemmer data og arbejde videre med det, HUSK at sæt all=TRUE, ellers mister du ', f(mister), ' rækker')
  } 

  # data
  if(all==FALSE) data <- data[1:n,]
  # data
  if(sample==TRUE) data <- data[sample(.N, n)]

  # system settings
  open_command <- switch(Sys.info()[['sysname']],
    Windows= 'open',
    Linux  = 'xdg-open',
    Darwin = 'open')
  temp_file <- paste0(tempfile(), '.xlsx')

  # lav workbook-objekter
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,'tmp')
  #lav en 'datatable' i excel - bedre visuelt, men maaske daarligt for kompabilitet? #OBS
  if(table==TRUE) { 
    openxlsx::writeDataTable(wb,'tmp',x=data, withFilter=filter, bandedRows=FALSE, bandedCols=TRUE, headerStyle=createStyle(textDecoration='bold')) 
  }
  if(table==FALSE) { 
    openxlsx::writeData(wb,'tmp',x=data, withFilter=filter) 
  }
  #goer column headers mere flexible (men ikke helt) og frys foerste raekke
  openxlsx::setColWidths(wb, 'tmp', cols =1:ncol(data), widths='auto')
  openxlsx::freezePane(wb, 'tmp', firstRow=TRUE)

  #gem og aabn
  openxlsx::saveWorkbook(wb, temp_file)
  system(paste0(open_command,' ',temp_file))
  
#   system(open_command,' ',temp_file))

# system("xdg-open /tmp/Rtmp6iWjEe/file63f27073576e.xlsx")



  # f_output <- data.frame(unclass(readxl::read_xlsx(temp_file)), check.names = FALSE) # data.frame (base-r)
  # f_output <- readxl::read_xlsx(temp_file) # tibble (ellers tak)
  f_output <- setDT(readxl::read_xlsx(temp_file)) # data.table
  f_output
}


