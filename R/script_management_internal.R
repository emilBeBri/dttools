#' short description 1
#'
#' @description descriptin 2??
#'
#' @import data.table
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export


script_management_internal <- function(dir='r/', extension='r') {
    # dir <- 'r/'
    # extension <- 'r'
    # scriptname='custom-functions'
    # fs::dir_ls('r')

    script <- NULL # programming with data.table
  if(!file.exists(dir %+% '00-trash')) {
    dir.create(dir %+% '00-trash')
    warning('00-trash fandtes ikke, laver den')
  }

  x <- data.table(script=list.files(dir))

  x1 <- x[grepl(paste0('\\.',extension), script, ignore.case=TRUE)]

  # makes it possible to see which files are different versions of the same script
  x1[,  family := gsub('v[0-9]{1,}', '', script)]
  x1[, version := gsub('.*(v[0-9]{1,}).*','\\1', script)]
  setorder(x1, family, -version, na.last=TRUE)
  x1 # output
}