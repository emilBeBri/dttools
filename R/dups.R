#' find dups in different ways
#'
#' these functions adds functionality lacking in data.table and other packages that are concerned with dups
#'
#' do different stuff, yes, that's what happens!
#' @param DT a data.table with suspcted dups
#' @param by the columns by which the dups should be detected
#' @param var should the output be a variabel that designates duplicates (TRUE) / unique(FALSE). If set, the function does NOT return an output, but modifices the data.table by reference and adds the variabel
#' @import data.table 
#' @importFrom utils tail
#' @export
#'
#' @examples
#' dups(dupstestdata)
#' @return This function returns a \code{data.table} with all the dups by the specified columns, instead of 'all the dups minus one' as is standard in data.table. also sets the key to the "by" variables for code efficiency.

# kunne evt laves med: 
# dtxc2 <- dtxc1[, if(.N == 1) .SD, by=person_id]
# koer benchmark paa den og se om det ikke er bedre? #todo#

# alle dups, ikke kun de naeste-efter-den-foerste
dups <- function(DT, by=colnames(DT), var=NULL) {
    # DT <- myDT
    # by=c('fB', 'fC')
  if(!is.data.table(DT)) {
    data.table::setDT(DT)
    warning('DT wasn\'t a data.table - turning it into one')
  }
  # beholder evt den gamle sortering hvis der er én (maaske ikke noedvendigt)
  # if( !is.null(key(DT)) ) {old_key <- key(DT)}
  #
  setkeyv(DT, by)
  tmp_dups = duplicated(DT, by = key(DT)) 
  if( is.null(var)){
    DT[tmp_dups | c(tail(tmp_dups, -1L), FALSE)]
  }  else{
    DT %>%  
    .[, c(var) := FALSE] %>%  
    .[tmp_dups | c(tail(tmp_dups, -1L), FALSE), c(var) := TRUE]
  }
}


