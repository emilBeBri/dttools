#' short description 1
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
#'
#' @import data.table
#' @importFrom magrittr "%>%"
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export

setspell <- function(DT, v1, idvar='pnr', place='') {
    # DT <- spell1[1:100000] 
    # idvar <- 'pnr'
    # v1 <- 'flag'
    # return='nomx'
    run_v1 <- spell_v1 <- NULL # programming with data.table

  org_cols <- copy(colnames(DT))  
  DT[, run_v1 :=  rleid(get(v1)), by=idvar][, spell_v1 := 1:.N, by=c(idvar,v1,'run_v1')] %>%   
  .[, `:=` (  mxrun_v1 = max(run_v1), mxspell_v1 = max(spell_v1) ), by=idvar] 

  stopifnot(place %in% c('', 'start'))

  if(place == 'start'){
    setcolorder(DT, c('run_v1', 'spell_v1', 'mxrun_v1','mxspell_v1'))
  } 

  setnames(DT, 
    c('run_v1', 'spell_v1', 'mxrun_v1','mxspell_v1'), 
    c('run_' %+% v1, 'spell_' %+% v1, 'mxrun_' %+% v1, 'mxspell_' %+% v1)
  )

}


#' short description 1
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
#'
#' @importFrom magrittr "%>%"
#' @import data.table
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export

setspell_selected <- function(DT, v1, idvar='pnr', value){
    # DT <- t1[, .(pnr, flag)]
    # idvar <- 'pnr'
    # v1 <- 'flag'
    # value <- TRUE
    run_v1 <- spell_v1 <- helper1 <- run_v1_2 <- spell_v1_2 <- NULL # programming with data.table


  org_cols <- copy(colnames(DT))  
  DT[, run_v1 :=  rleid(get(v1)), by=idvar][, spell_v1 := 1:.N, by=c(idvar,v1,'run_v1')] %>%   
  .[, `:=` (  mxrun_v1 = max(run_v1), mxspell_v1 = max(spell_v1) ), by=idvar] 


#  kun for specifikke udfald
tmp1 <- DT[, helper1 := .GRP, by=c(idvar,v1, 'run_v1')]
tmp1 <- tmp1[get(v1) %in% value]
tmp1[, run_v1_2 :=  rleid(run_v1), by=idvar][, spell_v1_2 := 1:.N, by=c(idvar,v1,'run_v1')] %>% 
.[, `:=` (  mxrun_v1_2 = max(run_v1_2), mxspell_v1_2 = max(spell_v1_2) ), by=idvar]

thecols <- setdiff(colnames(tmp1), colnames(DT))
DT[tmp1, (thecols) := mget(thecols), on=intersect(colnames(tmp1), colnames(DT))]
DT[, (c('helper1','run_v1', 'spell_v1', 'mxrun_v1', 'mxspell_v1')) := NULL]
setnames(DT, c('run_v1_2', 'spell_v1_2', 'mxrun_v1_2','mxspell_v1_2'), c('run_' %+% v1, 'spell_' %+% v1, 'mxrun_' %+% v1, 'mxspell_' %+% v1))

}



