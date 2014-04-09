\name{summary.NLSdata}
\alias{summary.NLSdata}
\title{Summarizing Generalized Linear Model Fits}

\description{Prints metadata and distributional information for all variables
  contained in an NLSdata object}

\usage{summary(nls.data)}
\arguments{
  \item{nls.data}{An object of class "NLSdata"}
}

\examples{
codebook <- system.file("Investigator", "Religion.cdb", package = "NLSdata")
csv.extract <- system.file("Investigator", "Religion.csv", package = "NLSdata")


nls.obj <- CreateNLSdata(codebook, csv.extract)
summary(nls.obj)}
