\name{summary.NLSdata}
\alias{summary.NLSdata}
\title{Summarizing Generalized Linear Model Fits}

\description{Prints metadata and distributional information for all variables
  contained in an NLSdata object}

\usage{\method{summary}{NLSdata}(object, ...)}
\arguments{
  \item{object}{An object of class "NLSdata"}
  \item{...}{Not used currently}
}

\examples{
codebook <- system.file("Investigator", "Religion.cdb", package = "NLSdata")
csv.extract <- system.file("Investigator", "Religion.csv", package = "NLSdata")


nls.obj <- CreateNLSdata(codebook, csv.extract)

\dontrun{
summary(nls.obj)
}
}
