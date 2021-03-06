% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esave.R
\name{esave}
\alias{esave}
\title{save objects with backup}
\usage{
esave(
  DT1,
  filename = "",
  filepath = "./",
  csv = FALSE,
  xlsx = FALSE,
  fst = FALSE,
  qs_preset = "archive",
  fst_preset = 100
)
}
\arguments{
\item{DT1}{an R object.}

\item{filename}{name of the file. Do not include a fileextension, these will be automatically appended depending on save functions used.}

\item{filepath}{filepath to save to. Default is the name of the saved object.}

\item{csv}{save as csv-file with fwrite.}

\item{xlsx}{save as xslx excel-sheet with writexl.}

\item{fst}{save as fst-file with fst.}

\item{qs_preset}{compression of qs file. One of "fast", "high" (default), "high", "archive", "uncompressed" or "custom". See details in qsave() function from the library(qs).}

\item{fst_preset}{compression of fst file. Value in the range 0 to 100, indicating the amount of compression to use. Lower values mean larger file sizes. The default compression is set to 50. See details in fst_write() function from the library(fst).}
}
\value{
This function returns \code{the url} blah blah blah
}
\description{
saves objects in different formats and automatically makes backups. Only tested on data.frames so far (Tue Nov 24 12:37:51 2020)
}
\examples{
\dontrun{
function(arg1)
}
}
