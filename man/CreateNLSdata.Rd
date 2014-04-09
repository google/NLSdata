\name{CreateNLSdata}
\alias{CreateNLSdata}
\alias{CreateNLSData}
\title{Constructor for NLSdata}
\description{Converts raw NLS Investigator output to an NLSdata object}
\usage{CreateNLSdata(codebook, csv.extract,
                     na.strings =  c("-1", "-2", "-3", "-4", "-5"))}

\arguments{
  \item{codebook}{the .cdb file from the NLS Investigator export}
  \item{csv.extract}{the .csv file from the NLS Investigator export}
   \item{na.strings}{which of the missing value situations to represent as NA}
}

\examples{
library(NLSdata)
codebook <- system.file("Investigator", "Religion.cdb", package = "NLSdata")
csv.extract <- system.file("Investigator", "Religion.csv", package = "NLSdata")


nls.obj <- CreateNLSdata(codebook, csv.extract)
class(nls.obj)

names(nls.obj)

head(nls.obj$data[order(nls.obj$data$PUBID.1997), c(2, 8, 9, 11)])
nls.obj$metadata[["YSAQ_282A2.2005"]]
}
  

