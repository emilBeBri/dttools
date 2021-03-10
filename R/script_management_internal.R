#' data.table with names of scripts and possible versions of scripts they belong to.
#'
#' @description data.table with names of scripts and possible versions of scripts they belong to.
#' 
#' @param dir directory to search in 
#' @param extension extension to look for 
#' 
#'
#' @import data.table
#'
#' @return This function returns a \code{data.table} with names of scripts found in the directory specified
#' @examples
#'\dontrun{
#' function(arg1)
#'}


script_management_internal <- function(dir='r/', extension='r') {
    # dir <- 'r/'
    # dir <- '/home/emil/Dropbox/Statistik_neworder/Projekter/testprojekt/r/'
    # extension <- 'rmd'
    script <-  NULL # programming with data.table

  # if user forgot '/', ad it to the path
  if( !grepl('/$', dir) ) dir <- dir %+% '/'


  if(!file.exists(dir %+% '00-trash')) {
    dir.create(dir %+% '00-trash')
    warning('00-trash fandtes ikke, laver den')
  }

  # list.files 
  x <- data.table(script=list.files(dir, pattern='\\.'))

  x1 <- x[grepl(paste0('\\.', extension, '$'), script, ignore.case=TRUE)]
  assert_that(nrow(x1) > 0, msg='no files found with that extension')


  # removes the version numbering in order to establish script family name
  x1[,  family := tolower(gsub('(.*)[-_ ]*v[0-9]+[-_ ]*(.*)', '\\1\\2', script, ignore.case=TRUE))] 
  # version and extension
  x1[grepl('v[0-9]+', script), version := as.integer(gsub('.*v([0-9]+).*','\\1', script))]
  x1[,  extension := tolower(gsub('.*\\.(.*)$', '\\1', script))][]

  setorder(x1, family, -version, na.last=TRUE)
  x1 # output
}




