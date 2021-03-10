#' opens data in spreadsheet program
#'
#' opens data in spreadsheet program2
#'
#' @param data a data.frame
#' @param filter should there be the autofilter function enabled in the spreadsheet?
#' @param table as a table?
#' @param n number of rows
#' @param all should all rows be used? if TRUE, overrules the n argument.
#' @param sample should the n rows be sampled of taken from the top down?
#' @param engine which package should be used for convertion to xlsx? default is readxl. openxlsx has better setup of view options (such as using the excel-table function for autosorting), but in some rare cases it will fail on encoding of characters, deleting all information in the process, with no warning (that I can figure out). So it's not default for that reason, but will probably work fine most of the time. Just be careful about data loss when using it. 
#' @import data.table 
#' @import readxl
#' @import openxlsx
#' @importFrom purrr map imap keep
#' @importFrom rlang set_names
#' @export
#'
#'
#'
#'@examples
#'\dontrun{
#' dat2 <- vi(  dupstestdata)
#'}
#' @return This function opens a \code{data.frame} in a spreadsheet viewer. if asssigned to an object, the data.frame as well as manual changes in the spreadsheet will be saved to that object. 


# se i excel ark
# todo: hvordan haandterer den faktorer? 
# todo: sæt WriteXLS pakken på også, den har autofiler (og kan måske klare det som den anden pakke ikke kan, måske)

# https://www.tadviewer.com/


v2 <- function(data, filter=TRUE, table=TRUE, n=9000, all=FALSE, sample=FALSE, engine='writexl') {
    # # test
    # data <- copy(net2_metrics)
    # filter <- TRUE
    # table <- TRUE
    # all <- TRUE
    # sample <- FALSE
    # n <- 9000
    # engine='tad'

  # check if vector()
  if(is.vector(data)==TRUE & is.data.frame(data)==FALSE) {
    data <- data.table::data.table(data)
  }  

  if(is.data.table(data) != TRUE){
    data <- data.table::as.data.table(data) #for en sikkerhedsskyld - tibbles failer fx. uden det her
  }

  if(all==TRUE & sample==TRUE) stop('du kan ikke *baade* sample *og* tage alle med')
  if(all == FALSE & nrow(data) > n){
    mister <-  nrow(data) - n
    warning('der er flere rows end n er sat til - hvis du gemmer data og arbejde videre med det, HUSK at saet all=TRUE, ellers mister du ', dttools::eu_format(mister), ' raekker')
  } 

  # data
  if(n > nrow(data)) n <- nrow(data)
  if(all==FALSE) data <- data[1:n,]
  # data
  if(sample==TRUE) data <- data[sample(.N, n)]

  # data integrity: fix issue of readxl and openxlsx converting type date into type POSixt, and factors into character - part 1
  data_classes_old <- unlist(sapply(data, class))

  data_factors <- names(data_classes_old[data_classes_old == 'factor'] )
  data_factor_levels_old <- map(set_names(data_factors), ~ {
      # .x <- data_factors[[1]]
    data[, levels(get(.x))]
  })


  if(engine %!in% c('openxlsx', 'writexl', 'tad')) stop('engine not supported')


  # openxlsx save to file
  if(engine == 'openxlsx'){
    # lav workbook-objekter
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb,'tmp')
    #lav en 'datatable' i excel - bedre visuelt, men maaske daarligt for kompabilitet? #OBS
    if(table==TRUE) { 
      openxlsx::writeDataTable(wb,'tmp',x=data, withFilter=filter, bandedRows=FALSE, bandedCols=TRUE, headerStyle=openxlsx::createStyle(textDecoration='bold')) 
    }
    if(table==FALSE) { 
      openxlsx::writeData(wb,'tmp',x=data, withFilter=filter) 
    }
    #goer column headers mere flexible (men ikke helt) og frys foerste raekke
    openxlsx::setColWidths(wb, 'tmp', cols =1:ncol(data), widths='auto')
    openxlsx::freezePane(wb, 'tmp', firstRow=TRUE)

    #gem og aabn
    openxlsx::saveWorkbook(wb, tmp_file)
    warning('openxlsx can in rare cases remove text because of encoding issues. be careful that something valuable is not lost.')
  }
  # writexl save to file
  if(engine == 'writexl') writexl::write_xlsx(data, tmp_file)


  # system settings
  # open with default spreadsheet program
  if(engine %in% c('openxlsx', 'writexl')) {
    open_command <- switch(Sys.info()[['sysname']],
      Windows= 'open',
      Linux  = 'xdg-open',
      Darwin = 'open'
    )
    tmp_file <- paste0(tempfile(), '.xlsx')
    system(paste0(open_command,' ',tmp_file))
  }

  if(engine == 'tad'){
    tmp_file <- paste0(tempfile(), '.csv')
    fwrite(data, tmp_file)
  }
  
  if(engine %in% c('tad')) system(paste0('tad',' ',tmp_file))




  # system("xdg-open /tmp/Rtmp6iWjEe/file63f27073576e.xlsx")

  # f_output <- data.frame(unclass(readxl::read_xlsx(tmp_file)), check.names = FALSE) # data.frame (base-r)
  # f_output <- readxl::read_xlsx(tmp_file) # tibble (ellers tak)
  f_output <- setDT(readxl::read_xlsx(tmp_file)) # data.table

  #######  #subsection ######
  # data integrity part2: 

  # fix issue of readxl and openxlsx converting type date into type POSixt
  thecols <- names(data_classes_old[data_classes_old=='Date'])
  # der findes nogle date-variable
  if( 
      exists('thecols')
      &
      length(thecols) > 0
  ){
    f_output[, (thecols) := map(.SD, ~{
      as.Date(.x)
    }), .SDcols = thecols]
  }

  thecols <- names(data_factor_levels_old)
  if(!!length(thecols)) {
    f_output[, (thecols) := imap(.SD, ~{
        # .x <- f_output[[thecols[1]]]
        # .y <- thecols[1]
      factor(.x, levels=data_factor_levels_old[[.y]])    
    }), .SDcols = thecols]
  }

  # change logical cols back to logical, BUT if they include values outside 1,
  # 0, NA, then don't change them and create a warning for the user that it's
  # probably an erroneous data entry
  k1 <- names(data_classes_old[data_classes_old == 'logical'])
  data_logical <- vector('list', length=length(k1))
  data_logical[seq_len(length(data_logical))] <- TRUE
  names(data_logical) <- k1
  for(xcol in k1){
      # xcol <- names(data_classes_old[data_classes_old == 'logical'])[[1]]
    if(all(unique(f_output[[xcol]]) %in% c(TRUE, FALSE, NA))) {
      f_output[[xcol]] <- as.logical(f_output[[xcol]])
    } else data_logical[xcol] <- FALSE
  } 
  # for the warning
  data_logical <- keep(data_logical, isFALSE)

  # # final check of data types, to display a warning in case of changes
  data_classes_new <- unlist(sapply(f_output, class))
  # in case that the order of the cols are different, match with colnames before checking if there are differences in type (potential for bugs here) #OBS#
  same_cols <- intersect(names(data_classes_old), names(data_classes_new))
  data_classes_old2 <- data_classes_old[names(data_classes_old) %in% same_cols]
  data_classes_old2 <- data_classes_old2[order(names(data_classes_old2))]
  data_classes_new2 <- data_classes_new[names(data_classes_new) %in%  same_cols]
  data_classes_new2 <- data_classes_new2[order(names(data_classes_new2))]

  data_classes_wrong_type <- data_classes_old2[data_classes_new2 != data_classes_old2]

  # 1.5%%1==0
  # f_output[, names(data_classes_wrong_type), with=FALSE]
  # kdf_tmp1[, names(data_classes_wrong_type), with=FALSE]

  ##### subsection over #####

  if(!!length(data_logical)){ 
    warning('A col of type logical had a data entry not in the range of c(1, 2, NA). converted to appropriate type (character or numerical)')
  }
  if(!!length(data_classes_wrong_type)){ 
    warning('Some cols do not have the same type before than after, check the following cols for consistency: \n', 
      paste(names(data_classes_wrong_type), collapse=' ')
  )
  }
 f_output[] # output
}

