#' file size, nicely formatted
#'
#' @description file size formatted smartly to use a unit appropiate for it's size
#'
#' @param size size to format 
#'
#'
#' @return This function returns \code{a character vector} with the size appropriately formatted. 
#' @examples
#'\dontrun{
#' file_size_formated(1:10)
#'}
#' @export

# from here
# https://stackoverflow.com/questions/63543853/how-can-we-get-the-formated-file-size-in-kb-mb-gb-tb-in-r
file_size_formated <- function(size){
  

  k = size/1024.0 ^ 1
  m = size/1024.0 ^ 2
  g = size/1024.0 ^ 3
  t = size/1024.0 ^ 4
  
    if (t > 1) {
      outSize = paste0(round(t,2),"TB")
    } else if (g > 1) {
      outSize = paste0(round(g,2),"GB")
    } else if (m > 1) {
      outSize = paste0(round(m,2),"MB")
    } else if (k > 1) {
      outSize = paste0(round(k,2),"KB")
    } else{
      outSize = paste0(round(size,2),"B")
    }
    
  outSize
}
# vectorize the function
file_size_formated <- Vectorize(file_size_formated)



