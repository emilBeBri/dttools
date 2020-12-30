#' save objects with backup 
#'
#' @description saves objects in different formats and automatically makes backups. Only tested on data.frames so far (Tue Nov 24 12:37:51 2020)
#'
#' @param DT1 an R object.  
#' @param filename name of the file. Do not include a fileextension, these will be automatically appended depending on save functions used.  
#' @param filepath filepath to save to. Default is the name of the saved object.  
#' @param csv save as csv-file with fwrite.
#' @param xlsx save as xslx excel-sheet with writexl.
#' @param fst save as fst-file with fst.
#' @param qs_preset compression of qs file. One of "fast", "high" (default), "high", "archive", "uncompressed" or "custom". See details in qsave() function from the library(qs).
#' @param fst_preset compression of fst file. Value in the range 0 to 100, indicating the amount of compression to use. Lower values mean larger file sizes. The default compression is set to 50. See details in fst_write() function from the library(fst).
#'
#' @importFrom fst write_fst
#' @importFrom qs qsave
#' @importFrom writexl write_xlsx
#' @importFrom purrr map_dbl
#' @import data.table
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' function(arg1)
#'}
#' @export


# test - der var en fejl, men den var der ikke alligevel??
# save clippings
# esave(clip_archive, './data/')
# test1 <- function(DT1, filename='', filepath='./') {
#   # filepath <- '.'
#   if(filename == '') filepath2 <- filepath %+% deparse(substitute(DT1))
#   if(filename != '') filepath2 <- filepath %+% filename
#   # filepath2 <- filepath %+% deparse(substitute(DT1))
#   qsave(DT1, filepath2  %+% '.qs', preset='archive')
# }

# test1(clip_archive, filepath='data/')
# esave(clip_archive, filepath='data/')

esave <- function(DT1, filename='', filepath='./', csv=FALSE, xlsx=FALSE, fst=FALSE, qs_preset='archive', fst_preset=100){  
    # DT1 <- copy(clip_archive)
    # filepath <- './data/'
    # qs_preset='archive'
    # filename=''
    # fst_preset = 100
    # fst=FALSE
    # xlsx=FALSE
    # csv=FALSE
  assert_that(filename %!like%  '/' , msg='looks you switched the filename and the filepath argument by mistake , since the filename argument contains a "/", which will lead to errors when writing to file. stopping here.')

  # nuvaerende dato i laesevenligt format
  present_date <- as.Date(as.POSIXct(substr(Sys.time(),1,10), format="%Y-%m-%d", tz=Sys.timezone()),tz=Sys.timezone())

  if(filename == '') filepath2 <- filepath %+% deparse(substitute(DT1))
  if(filename != '') filepath2 <- filepath %+% filename

  # qs
  qsave(DT1, filepath2  %+% '.qs', preset=qs_preset)

  if(fst==TRUE){
    write_fst(DT1, filepath2  %+%  '.fst', compress=fst_preset)
  }
  if(xlsx==TRUE){
    write_xlsx(DT1, filepath2 %+%  '.xlsx')
  }
  if(csv==TRUE){
    fwrite(DT1, filepath2 %+%  '.csv')
  }
  

  # all backups greater than ~ 100 MB
  # utils:::format.object_size(50000000, "auto")
  # utils:::format.object_size(sum(map_dbl(filepath2 %+% a1, ~ file.size(.x))), "auto")
  a1 <- c('.qs', '.csv', '.xlsx', '.fst')
  if(sum(map_dbl(filepath2 %+% a1, ~ file.size(.x)), na.rm=TRUE ) > 50000000) {
    warning('filstoerrelsen er ' %+% 
      e_internal_format.object_size(file.size(filepath2 %+% '.qs'), "auto") %+% '.' %+% ' De mange backups kommer til at tage meget plads. overvej om du har brug for dem allesammen.') 
  }


  # backup (kan tage to forskellige paa en dag)
  # der er vist lidt problemer her. tag lige og check den igennem.



  # qs backup
  if(file.exists(filepath2  %+% '_' %+%  present_date %+%   '_bak.qs') !=TRUE ) {
    qsave(DT1, filepath2  %+% '_' %+%  present_date %+%   '_bak.qs', preset=qs_preset)
  } else {
    qsave(DT1, filepath2  %+% '_' %+%  present_date %+%   '_bak2.qs', preset=qs_preset)
  }

  # fst backup
  if(file.exists(filepath2  %+% '_' %+%  present_date %+%   '_bak.fst') !=TRUE & fst == TRUE ) {
    write_fst(DT1, filepath2  %+% '_' %+%  present_date %+%   '_bak.fst', compress=fst_preset)
  } else if(fst == TRUE) {
    write_fst(DT1, filepath2  %+% '_' %+%  present_date %+%   '_bak2.fst', compress=fst_preset)
  }

  # xlsx backup
  if(file.exists(filepath2  %+% '_' %+%  present_date %+%   '_bak.xlsx') !=TRUE & xlsx == TRUE ) {
    write_xlsx(DT1, filepath2  %+% '_' %+%  present_date %+%   '_bak.xlsx')
  } else if(xlsx == TRUE) {
    write_xlsx(DT1, filepath2  %+% '_' %+%  present_date %+%   '_bak2.xlsx')
  }

  # csv backup
  if(file.exists(filepath2  %+% '_' %+%  present_date %+%   '_bak.csv') !=TRUE & csv == TRUE ) {
    fwrite(DT1, filepath2  %+% '_' %+%  present_date %+%   '_bak.csv')
  } else if(csv == TRUE) {
    fwrite(DT1, filepath2  %+% '_' %+%  present_date %+%   '_bak2.csv')
  }

}


