#' creates spells and run-lengths
#'
#' @description creates spells and run-lengths, by reference
#'
#' @param DT a data.table 
#' @param var1 variable to calcute run- and spell-lengths on.  
#' @param grp grouping variables within-which to calculate on 
#' @param pos where should the variables be located? 
#'
#' @import data.table
#' @importFrom magrittr "%>%"
#'
#' @return This function creates, by reference, four variables with run- and spell-length-type information. 
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export




setspell <- function(DT, var1, grp ,pos='end') {
    # DT <- copy(dtx) 
    # var1 <- 'var1_num'
    # grp <- c('grp1','grp2')
    # pos='grp1'

    # programming with data.table
    run_var1 <- spell_var1 <- NULL 

  if(!is.data.table(DT)) setDT(DT)

  # columns 
  org_cols <- copy(names(DT))  
  spell_cols <- c('run_var1', 'spell_var1', 'mxrun_var1','mxspell_var1')
  final_spell_cols <- c('run_', 'spell_', 'mxrun_', 'mxspell_') %+% var1
  
  # do the names of the spell cols already exists? if so, delete
  already_in_dt <- c(
    final_spell_cols[final_spell_cols %in% names(DT)],
    spell_cols[spell_cols %in% names(DT)])
  if( !!length(already_in_dt) ) {
    DT[ , (already_in_dt) := NULL]
    # update original colnames, now without the variables. otherwise, insert_at() throws an error down below.
    org_cols <- copy(names(DT))  
    warning('spell- and run-length-variables already exists. deleted and replaced.')
  } 

  # has to be in the beginning, or the , end or after a column
  stopifnot(pos %in% c('end', 'start', names(DT)))

  DT %>%   
  .[, run_var1 :=  rleid(get(var1)), by=grp] %>%   
  .[, spell_var1 := 1:.N, by=c(grp, var1, 'run_var1')] %>%   
  .[, `:=` (  mxrun_var1 = max(run_var1), mxspell_var1 = max(spell_var1) ), by=grp] 

  if(pos == 'start'){
    setcolorder(DT, spell_cols)
  }
  if( pos %in% names(DT)){
    setcolorder(DT,
      insert_at(org_cols, which(org_cols == pos), spell_cols)
    )
  }


  setnames(DT, 
    spell_cols,
    final_spell_cols
  )[0]
  return(message(''))

  # the [0] and return message are in order to avoid the problem with by-reference in data.table functions, where, the first time you print to console afterwards, nothing happens. Not an ideal solution, but it works.

}

# dtx <- data.table(read_xlsx('/home/emil/Desktop/Untitled 1.xlsx'))
# setspell(dtx, 'var1_num', qc(grp1, grp2))
# setspell(dtx, 'var1_num', qc(grp1, grp2), pos='grp1')
# v(dtx)


# not ready yet - find out how to create internal functions, #todo#

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

setspell_selected <- function(DT, var1, grp, value){
    DT <- copy(dtx)
    grp <- 'grp1'
    var1 <- 'var1_num'
    value <- 'A'
    # programming with data.table
    run_var1 <- spell_var1 <- helper1 <- run_var1_2 <- spell_var1_2 <- NULL 

  org_cols <- copy(names(DT))  
  setspell(DT, var1=var1, grp=grp)

  #  
  tmp1 <- DT[, helper1 := .GRP, by=c(grp,var1, 'run_var1')]
  tmp1 <- tmp1[get(var1) %in% value]
  tmp1[, run_var1_2 :=  rleid(run_var1), by=grp][, spell_var1_2 := 1:.N, by=c(grp,var1,'run_var1')] %>% 
  .[, `:=` (  mxrun_var1_2 = max(run_var1_2), mxspell_var1_2 = max(spell_var1_2) ), by=grp]

  thecols <- setdiff(names(tmp1), names(DT))
  DT[tmp1, (thecols) := mget(thecols), on=intersect(names(tmp1), names(DT))]
  DT[, (c('helper1','run_var1', 'spell_var1', 'mxrun_var1', 'mxspell_var1')) := NULL]
  setnames(DT, c('run_var1_2', 'spell_var1_2', 'mxrun_var1_2','mxspell_var1_2'), c('run_' %+% var1, 'spell_' %+% var1, 'mxrun_' %+% var1, 'mxspell_' %+% var1))


  v(DT)
}



