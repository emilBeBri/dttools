#' short description 1
#'
#' @description this is just a copy of utils:::format.object_size(), which is an internal function to the base R utils package. You're not allowed to use internal functions from other packages, i.e. the "..." operator, at least not if you want your package on CRAN. To come around this, I just copied the function here.
#'


e_internal_format.object_size <- function (x, units = "b", standard = "auto", digits = 1L, ...) {
    known_bases <- c(legacy = 1024, IEC = 1024, SI = 1000)
    known_units <- list(SI = c("B", "kB", "MB", "GB", "TB", "PB", 
        "EB", "ZB", "YB"), IEC = c("B", "KiB", "MiB", "GiB", 
        "TiB", "PiB", "EiB", "ZiB", "YiB"), legacy = c("b", "Kb", 
        "Mb", "Gb", "Tb", "Pb"), LEGACY = c("B", "KB", "MB", 
        "GB", "TB", "PB"))
    units <- match.arg(units, c("auto", unique(unlist(known_units), 
        use.names = FALSE)))
    standard <- match.arg(standard, c("auto", names(known_bases)))
    if (is.null(digits)) 
        digits <- 1L
    if (standard == "auto") {
        standard <- "legacy"
        if (units != "auto") {
            if (endsWith(units, "iB")) 
                standard <- "IEC"
            else if (endsWith(units, "b")) 
                standard <- "legacy"
            else if (units == "kB") 
                stop("For SI units, specify 'standard = \"SI\"'")
        }
    }
    base <- known_bases[[standard]]
    units_map <- known_units[[standard]]
    if (units == "auto") {
        power <- if (x <= 0) 
            0L
        else min(as.integer(log(x, base = base)), length(units_map) - 
            1L)
    }
    else {
        power <- match(toupper(units), toupper(units_map)) - 
            1L
        if (is.na(power)) 
            stop(gettextf("Unit \"%s\" is not part of standard \"%s\"", 
                sQuote(units), sQuote(standard)), domain = NA)
    }
    unit <- units_map[power + 1L]
    if (power == 0 && standard == "legacy") 
        unit <- "bytes"
    paste(round(x/base^power, digits = digits), unit)
}




#' short description 1
#'
#' @description descriptin 2??
#'
#' @param argument1 arg-description 
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

# #obs#
# "The use of unexported/internal functions called via ::: is not allowed by CRAN."
# dvs hvis pakken skal paa CRAN saa skal du uden om at bruge den interne utils:::format.object_size() funktion.

esave <- function(DT1, filename='', filepath='./', csv=TRUE, xlsx=FALSE, fst=TRUE, qs_preset='archive', fst_preset=100){  
    # DT1 <- copy(kdf)
    # filepath <- './tmp-data/'
    # qs_preset='archive'
    # filename=''
    # filename='test2'
    # fst_preset = 100
    # fst=FALSE
    # xlsx=FALSE
    # csv=FALSE

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


