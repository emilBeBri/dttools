#' closes all open data viewers in linux 
#'
#' @description requires wmctrl to be installed. sudo apt-get install wmctrl in Ubuntu (and perhaps also other debian-based systems) 
#'
#' @param argument1 arg-description 
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export

cv <- function(){
    cmd <- paste0('wmctrl -c "Data:" -v')
    ok <- TRUE
    while(ok){
        out <- suppressWarnings(system(cmd,intern=TRUE,ignore.stderr=TRUE))
        Sys.sleep(0.1)
        ok <- is.null(attr(out,"status"))
        print(ok)
    }
}
