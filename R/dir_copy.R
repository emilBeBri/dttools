##' copy directoy 
##'
##' @description Copy a directory. Max / Linux only for now
##'
##' @param from from which folder 
##' @param to to which folder
##'
##'
##' @return This function copies a directory to another location.
##' @examples
##'\dontrun{
##' dir_copy('/from/here/', '/to/here/')
##'}
##' @export

# from:
# https://stackoverflow.com/questions/32453455/copy-folder-recursive-in-r

# dir_copy <- function(from, to) {
#   os <- Sys.info()['sysname']
#   if (os == "Darwin" || os == "Linux") {
#     command <- sprintf("cp -R '%s' '%s'", from, to)
#     system(command, intern = TRUE)   
#   } else stop('wrong filesystem. Has to be Linux or OSX.')
# }



