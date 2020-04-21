
# ikke så vigtig lige nu - skriv til clipboard
# #' @export
# # write to clip manager
# wc <- function(df1, var, n=10,all=FALSE, sample=TRUE) {
#   # require(clipr)
#   if(all==TRUE) n <- nrow(df1)

#   if(sample==TRUE) {
#     clipr::write_clip(
#     df1[seq(1,nrow(df1),ceiling((nrow(df1)/n))), get(var)]
#     , col.names=F)
#   } else clipr::write_clip(df1[1:n, get(var)] , col.names=F)
# }


# åbn i r viewer
#' @export
v <- function(DT, all=FALSE, n=1000) {
	# if a variable in the DT is already called n, there will be a problem. so let's rename n right away
	n_number <- n 
	n <- NULL
  # DT= 1:5
  if(is.data.frame(DT)==FALSE) DT <- data.frame(DT)
  if(nrow(DT)<n_number) n_number <- nrow(DT)
  if(all==FALSE) utils::View(DT[1:n_number,]) else utils::View(DT)
}

