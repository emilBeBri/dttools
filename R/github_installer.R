#' installs a R package from github
#'
#' installs a R package from github
#'
#' installs a R package from github
#' @param url link to a github r package- 
#'
#' @examples
#' github_installer('emilbebri/dttools')
#' @return This function  \code{the url} that was given to it, after it has installed the package
#' @importFrom pkgbuild build
#' @importFrom utils install.packages
#' @export

github_installer <- function(url=NULL, ... ) {
    # url <- 'emilbebri/dttools'
	   
  if( is.null(url))	stop('no url chosen')

  url <- tolower(url)
  packagename <- gsub('.*/(.*$)', '\\1', url)
  url <- paste0('https://github.com/', url, '/archive/master.zip')


  tpath <- tempdir()

  old_path <- getwd()

  on.exit(setwd(old_path), add=TRUE) #reset wd if errir before abort

  on.exit(unlink(tpath, recursive = TRUE, force = TRUE), add=TRUE) #delete temp files

  fname <- paste0(tpath,"/blop.zip")

  utils::download.file(url=url, fname)

  utils::unzip(fname,exdir=tpath)

  pname <- list.files(tpath)[grep(packagename, list.files(tpath))]

  pname <- paste0(tpath,"/",pname)

  setwd(tpath)

  bpath <- paste0(tpath,"/bin")

  dir.create(bpath)

  pkgbuild::build(pname, vignettes = FALSE, dest_path = bpath)

  bname <- list.files(bpath,full.names = TRUE)[1]

  install.packages(pkgs=bname,repos=NULL,method="source", ...)

  invisible(url)

}



# to-do: 
# lav den evt så den kan bruges til gamle versioner af pakker også, der ligger på git

