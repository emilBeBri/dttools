##############  #afsnit #################
# put into package

setwd('/home/emil/Dropbox/Statistik_neworder/R/R-package-development/dttools')

dst_fornavne <- readRDS('/home/emil/Dropbox/Statistik_neworder/R/R-package-development/dttools/data-raw//fornavne-fra-dst-1985-2018.rds')

# Encoding(levels(dst_fornavne$fornavn)) <- "latin1"

# t1 <-  which(grepl('(?i)å',c1$fornavn,perl=T))
# t2 <- c1[t1] 

# c1[t1]


Encoding(dst_fornavne$fornavn) <- "UTF-8"
dst_fornavne$fornavn <- iconv(
  dst_fornavne$fornavn, 
  "UTF-8", 
  "UTF-8"
)
stringi::stri_escape_unicode('ø')



# export to package
usethis::use_data(dst_fornavne, overwrite = TRUE)


# readRDS('/home/emil/Dropbox/Statistik_neworder/R/R-package-development/dttools/data/dst_fornavne.rds')

# load('/home/emil/Dropbox/Statistik_neworder/R/R-package-development/dttools/data/dst_fornavne.rda')
# dst_fornavne





