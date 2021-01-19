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
#' @export


# #todo#
# use fs-package instead of list.files

script_management_internal <- function(dir='r/', extension='r') {
    # dir <- 'r/'
    # extension <- 'r'
    # fs::dir_ls('r')

    script <- NULL # programming with data.table
  if(!file.exists(dir %+% '00-trash')) {
    dir.create(dir %+% '00-trash')
    warning('00-trash fandtes ikke, laver den')
  }

  x <- data.table(script=list.files(dir))

  x1 <- x[grepl(paste0('\\.',extension), script, ignore.case=TRUE)]

  # makes it possible to see which files are different versions of the same script
  x1[,  family := gsub('[-]*v[0-9]{1,}\\.[rR]$', '', script)]
  x1[,  extension := gsub('.*\\.([rR])$', '\\1', script)]
  x1[, version := gsub('.*(v[0-9]{1,}).*','\\1', script)][]
  setorder(x1, family, -version, na.last=TRUE)
  x1 # output
}