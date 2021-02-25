#' creates spells and run-lengths
#'
#' @description creates spells and run-lengths, by reference
#'
#' @param dtx A data.table 
#' @param var1 Variable to calcute run- and spell-lengths on.  
#' @param grp Grouping variables within-which to calculate on 
#' @param pos Where should the variables be located in the data.table? 
#' @param rename Should the spells and run-lengths be named after the original variable or generically named? (mostly used as TRUE with the setspell_selected variable)
#' @param max Should the DT include the max run and spell lengths?
#'
#' @import data.table
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
#'
#' @return This function creates, by reference, four variables with run- and spell-length-type information. 
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export


setspell <- function(dtx, grp, var1, pos='end', max=TRUE) {
    # testdata_setspell1 <- data.table(group=qc(a,a,a,a,b,b,b,c,c,c,c), variabel1=c(1,2,NA,2,1,2,1,1,1,2,2))
    # dtx <- copy(a1)
    # grp <- 'group'
    # var1 <- 'variabel1'
    # pos <- 'end'
    # max <- TRUE


    # programming with data.table
    run_var1 <- spell_var1 <- NULL 

  assert_that( all(c(grp, var1) %in% names(dtx)), msg='variable does not exist')
  # positions of spell-cols has to be in the beginning, or the end, or after a column
  assert_that(pos %in% c('end', 'start', names(dtx)))
  
  if( !is.data.table(dtx)) setDT(dtx)

  # column names, with and without max spells and runs 
  org_cols <- copy(names(dtx))  
 
  spell_cols <- c('run_var1', 'spell_var1')
  if( max == TRUE) spell_cols <- c(spell_cols, 'mxrun_var1','mxspell_var1') 
 
  final_spell_cols <- c('run_', 'spell_') 
  if( max == TRUE) final_spell_cols <- c(final_spell_cols, 'mxrun_', 'mxspell_')
  final_spell_cols <- final_spell_cols %+% var1
  
  # do the names of the spell cols already exists? if so, delete
  already_in_dtx <- intersect(
    names(dtx),
    c(
      c('run_var1', 'spell_var1', 'mxrun_var1','mxspell_var1'),
      c('run_', 'spell_', 'mxrun_', 'mxspell_') %+% var1 
    )
  )

  if( !!length(already_in_dtx) ) {
    dtx[ , (already_in_dtx) := NULL]
    # update original colnames, now without the variables. otherwise, insert_at() throws an error down below.
    org_cols <- copy(names(dtx))  
    warning('spell- and run-length-variables already exists. deleted and replaced.')
  } 

  # spells and run lengths
  dtx %>%   
  .[, run_var1 :=  rleid(get(var1)), by=grp]  %>%   
  .[, spell_var1 := 1:.N, by=c(grp, var1, 'run_var1')]
  # max spells and run lengths within each group
  if( max ) dtx[, `:=` (  mxrun_var1 = max(run_var1, na.rm=TRUE), mxspell_var1 = max(spell_var1, na.rm=TRUE) ), by=grp]


  # if some other position than 'end' exists, place the new spell cols there
  if(pos == 'start'){
    setcolorder(dtx, spell_cols)
  }
  if( pos %in% names(dtx)){
    setcolorder(dtx,
      insert_at(org_cols, which(org_cols == pos), spell_cols)
    )
  }

  setnames(dtx, spell_cols, final_spell_cols)

  dtx[0]
  message('')
  # the [0] and return message are in order to avoid the problem with by-reference in data.table functions, where, the first time you print to console afterwards, nothing happens. Not an ideal solution, but it works.

}

# dtx <- data.table(read_xlsx('/home/emil/Desktop/Untitled 1.xlsx'))
# setspell(dtx, 'var1_num', qc(grp1, grp2))
# setspell(dtx, 'var1_num', qc(grp1, grp2), pos='grp1')
# v(dtx)


#' creates spells and run-lengths
#'
#' @description creates spells and run-lengths, by reference
#'
#' @param DT a data.table 
#' @param var1 variable to calcute run- and spell-lengths on.  
#' @param grp grouping variables within-which to calculate on 
#' @param pos where should the variables be located in the data.table? 
#' @param rename should the spells and run-lengths be named after the original variabel or genericaly named?
#'
#' @import data.table
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
#'
#' @return This function creates, by reference, four variables with run- and spell-length-type information. 
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export

setspell_selected <- function(dtx, grp, var1, value, check_value=TRUE, max=TRUE, pos='end'){
    # testdata_setspell1 <- data.table(group=qc(a,a,a,a,b,b,b,c,c,c,c), variabel1=c(1,2,NA,2,1,2,1,1,1,2,2))
    # dtx <- copy(a1)
    # grp <- 'group'
    # var1 <- 'variabel1'
    # value <- 2
    # check_value <- TRUE
    # pos <- 'end'
    # max <- TRUE
    # programming with data.table
    run_var1 <- spell_var1 <- helper1 <- run_var1_2 <- spell_var1_2 <- NULL 

  if(check_value) assert_that(all(value %in% unique(dtx[[var1]])) )

  assert_that( all(c(grp, var1) %in% names(dtx)), msg='variable does not exist')
  # positions of spell-cols has to be in the beginning, or the end, or after a column
  assert_that(pos %in% c('end', 'start', names(dtx)))
  
  if( !is.data.table(dtx)) setDT(dtx)

  # column names, with and without max spells and runs 
  org_cols <- copy(names(dtx))  
 
  spell_cols <- c('run_var1_2', 'spell_var1_2')
  if( max == TRUE) spell_cols <- c(spell_cols, 'mxrun_var1_2','mxspell_var1_2') 
 
  final_spell_cols <- c('run_', 'spell_') 
  if( max == TRUE) final_spell_cols <- c(final_spell_cols, 'mxrun_', 'mxspell_')
  final_spell_cols <- final_spell_cols %+% var1
  
  # do the names of the spell cols already exists? if so, delete
  already_in_dtx <- intersect(
    names(dtx),
    c(
      c('run_var1'),
      c('run_var1_2', 'spell_var1_2', 'mxrun_var1_2','mxspell_var1_2'),
      c('run_', 'spell_', 'mxrun_', 'mxspell_') %+% var1 
    )
  )

  if( !!length(already_in_dtx) ) {
    dtx[ , (already_in_dtx) := NULL]
    # update original colnames, now without the variables. otherwise, insert_at() throws an error down below.
    org_cols <- copy(names(dtx))  
    warning('spell- and run-length-variables already exists. deleted and replaced.')
  } 


  # rleid can't group by two variables, so we need to first create a run length and then make a run length on that run length - in case two runs are separated in the original data, but will look as if they are part of the same run when we look at them filtered only on these cases.
  dtx %>%   
  .[, run_var1 :=  rleid(get(var1)), by=grp] %>% 
  .[get(var1) %in% value, run_var1_2 :=  rleid(run_var1), by=c(grp)] %>%   
  .[get(var1) %in% value, spell_var1_2 := 1:.N, by=c(grp, var1, 'run_var1')] 
  # max spells and run lengths within each group
  if( max ) dtx[, `:=` (  mxrun_var1_2 = max(run_var1_2, na.rm=TRUE), mxspell_var1_2 = max(spell_var1_2, na.rm=TRUE) ), by=grp]
  # delete temporary rle 
  dtx[, ('run_var1') := NULL]

  # if some other position than 'end' exists, place the new spell cols there
  if(pos == 'start'){
    setcolorder(dtx, spell_cols)
  }
  if( pos %in% names(dtx)){
    setcolorder(dtx,
      insert_at(org_cols, which(org_cols == pos), spell_cols)
    )
  }

  setnames(dtx, spell_cols, final_spell_cols)

  dtx[0]
  message('')
  # the [0] and return message are in order to avoid the problem with by-reference in data.table functions, where, the first time you print to console afterwards, nothing happens. Not an ideal solution, but it works.

}


# testdata_setspell1 <- data.table(group=qc(a,a,a,a,b,b,b,c,c,c,c), variabel1=c(1,2,NA,2,1,2,1,1,1,2,2))
# setspell_selected(testdata_setspell1, var1='variabel1', grp='group',value=2)


# setspell_selected_bak <- function(dtx, var1, grp, value, max=TRUE, check_value=TRUE){
#     # testdata_setspell1 <- data.table(group=qc(a,a,a,a,b,b,b,c,c,c,c), variabel1=c(1,2,NA,2,1,2,1,1,1,2,2))
#     # dtx <- copy(testdata_setspell1)
#     # grp <- 'group'
#     # var1 <- 'variabel1'
#     # value <- 2
#     # check_value <- TRUE
#     # programming with data.table
#     run_var1 <- spell_var1 <- helper1 <- run_var1_2 <- spell_var1_2 <- NULL 

#   if(check_value) assert_that(all(value %in% unique(dtx[[var1]])) )

#   org_cols <- copy(names(dtx))  



#   # rleid can't group by two variables, so we need to first create a run length and then make a run length on that run length - in case two 
#   dtx %>%   
#   .[, run_var1 :=  rleid(get(var1)), by=grp] %>% 
#   .[get(var1) %in% value, run_var1_2 :=  rleid(run_var1), by=c(grp)] %>%   
#   .[get(var1) %in% value, spell_var1_2 := 1:.N, by=c(grp, var1, 'run_var1')] 
#   # max spells and run lengths within each group
#   if( max ) dtx[, `:=` (  mxrun_var1_2 = max(run_var1_2, na.rm=TRUE), mxspell_var1_2 = max(spell_var1_2, na.rm=TRUE) ), by=grp]
#   # delete temporary rle 
#   dtx[, ('run_var1') := NULL]


#   setnames(dtx, c('run_var1_2', 'spell_var1_2', 'mxrun_var1_2','mxspell_var1_2'), c('run_' %+% var1, 'spell_' %+% var1, 'mxrun_' %+% var1, 'mxspell_' %+% var1))

# }


# testdata_setspell1 <- data.table(group=qc(a,a,a,b,b,b,b,c,c), variabel1=c(1,2,2,NA,3,3,1,1,2))
# setspell(testdata_setspell1, 'variabel1', 'group')
# setspell(testdata_setspell1, 'variabel1', 'group', max=FALSE)


# a1 <- copy(t3)
# setspell(a1, 'N', 'grp')
# df <- structure(list(time = structure(c(1538876340, 1538876400, 
# 1538876460,1538876520, 1538876580, 1538876640, 1538876700, 1538876760, 1526824800, 
# 1526824860, 1526824920, 1526824980, 1526825040, 1526825100), class = c("POSIXct", 
# "POSIXt"), tzone = "UTC"), group = c("A", "A", "A", "A", "A", "A", "A", "A", "B", 
# "B", "B", "B", "B", "B"), is_5 = c(0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1)), 
# class = c("data.frame"), row.names = c(NA, -14L))
# is.data.table(df)
# tst1 <- as.data.table(df)
# is.data.table(tst1)
# setspell(tst1, 'group', 'is_5', max=FALSE)
# setspell(tst1, 'group', 'is_5', max=FALSE)
# setspell(tst1, 'group', 'is_5', max=TRUE)
# tst1
# setspell(tst1, 'group', 'is_5', max=FALSE)


# g1 <- data.frame(1:10)

# is.data.table(df)



# setspell(a1, 'N', 'grp', rename=t) 
# v2(a1)





# # setspell_selected2 <- function(dtx, var1, grp, value, check_value=TRUE){
# #     # testdata_setspell1 <- data.table(group=qc(a,a,a,a,b,b,b,c,c,c,c), variabel1=c(1,2,NA,2,1,2,1,1,1,2,2))
# #     # dtx <- copy(testdata_setspell1)
# #     # grp <- 'group'
# #     # var1 <- 'variabel1'
# #     # value <- 2
# #     # check_value <- TRUE
# #     # programming with data.table
# #     run_var1 <- spell_var1 <- helper1 <- run_var1_2 <- spell_var1_2 <- NULL 

# #   if(check_value) assert_that(all(value %in% unique(dtx[[var1]])) )

# #   org_cols <- copy(names(dtx))  

# #   # rleid can't group by two variables, so we need to first create a run 
# #   setspell(dtx, var1=var1, grp=grp, rename=FALSE, max=FALSE)


# #   tmp1 <- dtx[get(var1) %in% value]
# #   tmp1 %>%   
# #   .[, run_var1_2 :=  rleid(run_var1), by=grp] %>%
# #   .[, spell_var1_2 := 1:.N, by=c(grp, var1, 'run_var1')] 
# #   # .[, `:=` (  mxrun_var1_2 = max(run_var1_2, na.rm=TRUE), mxspell_var1_2 = max(spell_var1_2, na.rm=TRUE) ), by=grp]


# #   thecols <- setdiff(names(tmp1), names(dtx))
# #   dtx[tmp1, (thecols) := mget(thecols), on=intersect(names(tmp1), names(dtx))]


# #   dtx[, (c('run_var1', 'spell_var1', 'mxrun_var1', 'mxspell_var1')) := NULL]
  



# #   # setnames(dtx, c('run_var1_2', 'spell_var1_2', 'mxrun_var1_2','mxspell_var1_2'), c('run_' %+% var1, 'spell_' %+% var1, 'mxrun_' %+% var1, 'mxspell_' %+% var1))


# #   # v(dtx)


  
# # }


# # setspell_selected <- function(dtx, var1, grp, value, check_value=TRUE){
# #     # testdata_setspell1 <- data.table(group=qc(a,a,a,a,b,b,b,c,c,c,c), variabel1=c(1,2,NA,2,1,2,1,1,1,2,2))
# #     # dtx <- copy(testdata_setspell1)
# #     # grp <- 'group'
# #     # var1 <- 'variabel1'
# #     # value <- 2
# #     # check_value <- TRUE
# #     # programming with data.table
# #     run_var1 <- spell_var1 <- helper1 <- run_var1_2 <- spell_var1_2 <- NULL 

# #   if(check_value) assert_that(all(value %in% unique(dtx[[var1]])) )

# #   org_cols <- copy(names(dtx))  

# #   # rleid can't group by two variables, so we need to first create a runlength  
# #   setspell(dtx, var1=var1, grp=grp, rename=FALSE, max=FALSE)
# #   dtx[get(var1) %in% value, run_var1_2 :=  rleid(run_var1), by=c(grp)] %>%   
# #   .[get(var1) %in% value, spell_var1_2 := 1:.N, by=c(grp, var1, 'run_var1')] 


# # }



