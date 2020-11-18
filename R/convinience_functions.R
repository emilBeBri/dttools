#' convinience funktioner, kraever ikke rigtig dokumentation

#' @export
# NA fjernes automatisk
esum <- function(x, ...) round(sum(x,na.rm=TRUE), ...)

#' @export
emean <- function(x, ...) round(mean(x,na.rm=TRUE), ...)

#' @export
# vend om paa x og y i setdiff
setdiff2 <- function(x,y, ...) setdiff(y,x, ...)

# colnames paa sets
#' @export
setdiffcn <- function(x,y) setdiff(colnames(x),colnames(y))
#' @export
setdiffcn2 <- function(x,y) setdiff2(colnames(x),colnames(y))
#' @export
unioncn <- function(x,y) union(colnames(x),colnames(y))
#' @export
all.equalcn <- function(x,y) all.equal(colnames(x),colnames(y))
#' @export
intersectcn <- function(x,y) intersect(colnames(x),colnames(y))



#' @export
p <- function(df1, n=40){
  print(df1,topn=n)
} 

#' @export
l <- function(x) {
  length(x)
}
#' @export
lcn <- function(df) {
  length(colnames(df))
}

#' @export
nc <- function(df) {
  ncol(df)
}

#' @export
nr <- function(df) {
  nrow(df)
}

#' nrows for one data.frame minus the nrows of another data.frame 
#'
#' that's about it
#'
#' not sure about this right here
#' @param dtx A data.frame
#' @param dty A data.frame
#' @export
#'
#' @examples
#'\dontrun{
#' nr2(dtx, dtx)
#' }
#' @return This function returns an \code{integer} value with the difference in number of rows between the two data.frames (dtx-dty)  
#' 
nr2 <- function(dtx, dty, mode='-') {
  if(mode=='-') return(nrow(dtx) - nrow(dty) )
  if(mode=='+') return(nrow(dtx) + nrow(dty) )
}


# sammenlign antal raekker i to DTs
nrc <- function(DT1, DT2) {
  f(nrow(DT1) - nrow(DT2))
}



#' @export
cn <- function(df) {
  colnames(df)
}

#' @export
rn <- function(df) {
  rownames(df)
}

#' @export
s <- function(x) {
  sort(x)
}
#' @export
su <- function(x) {
  sort(unique(x))
}
#' @export
scn <- function(df) {
  sort(colnames(df))
}
#' @export
u <- function(x) {
  unique(x)
}
#' @export
lu <- function(x) {
  length(unique(x))
}

#' @export
l.f <- function(x) {
  list.files(x)
}




##############  #afsnit #################
# functions that needs more explaining
#########################################


#' are all rows unique, TRUE/FALSE
#'
#' are all rows unique, TRUE/FALSE. 
#'
#' are all rows unique, TRUE/FALSE
#' @param DT a data.frame
#' @import data.table 
#' @export
#'
#'
#'
#' @return This function returns a \code{logical value} TRUE if all rows are unique, FALSE if they are not. ...  is passed on to uniqueN from data.table, where the columns to check uniqueness to can be supplied as well. 

# Are all rows unique TRUE/FALSE. all arguments passed to data.tables uniqueN().
uniqueA <- function(DT, ...) {
    # require(data.table)
  data.table::uniqueN(DT, ...) == nrow(DT)  
}



