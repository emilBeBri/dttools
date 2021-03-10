#' common file-operations-function
#'
#' @description creates a data.table with enhanced file-information from a directory. additional arguments are passed to dir_info from the fs-library.
#'
#' @param dir directory to data.tablize 
#'
#' @import data.table
#' @import fs
#'
#' @return This function returns a \code{data.table} with file-information.
#' @examples
#'\dontrun{
#' dir_table('/path/to/folder')
#'}
#' @export


dir_table <- function (dir='', ...){
    # dir <- '~/Dropbox/bibliotek/'
    # programming with data.table
    name <- file <- size_char <- size <- ext <- only_path <- type <- modification_time <- NULL 
  x1 <- setDT(dir_info(dir, ...))
  # x1 <- setDT(dir_info(dir))
  x1[, file := path_file(path)]
  # only the path of a file (useful for renaming operations, for example). meta-characters for gsub escaped with Perl magic.
  anonfun1<- Vectorize(function(.x  ) gsub('\\Q' %+%path_file(.x) %+% '\\E' , '', .x, perl=TRUE))
  x1[, only_path := anonfun1(path)]
  # only assign file extension to non-directories and files with a dot in them
  x1[type != 'directory' & file %flike% '.', ext := gsub('.*\\.(.*$)', '\\1', path_file(path))]
  # function that gives size-information on dirs. 
  anonfun1 <- Vectorize(function(.x) sum(dir_info(.x, recurse = TRUE)$size))
  x1[type == 'directory', size := anonfun1(path)][]
  x1[, size_char := file_size_formated(size)]
  # name sans extension
  x1[file %flike% '.', name := gsub('(.*)\\..*$', '\\1', file)]
  x1[file %!flike% '.', name := file]
  # ordered by most used
  setcolorder(x1, qc(path, file, size, size_char, ext, modification_time, name))
  x1[]
}



