#' convinience funktioner, kræver ikke rigtig dokumentation

#' @export
# NA fjernes automatisk
esum <- function(x, ...) round(sum(x,na.rm=TRUE), ...)

#' @export
emean <- function(x, ...) round(mean(x,na.rm=TRUE), ...)

#' @export
# vend om på x og y i setdiff
setdiff2 <- function(x,y, ...) setdiff(y,x, ...)

# colnames på sets
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
