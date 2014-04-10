\name{RosterToLongDf}
\alias{RosterToLongDf}
\title{Conversion of Roster Variables to Long Format}
\description{The NLS has roster variables, variable dimension arrays which hold
  items like jobs that a person may have had over a year. RosterToLongDf turns a
  set of roster variables in a wide data frame into a long data frame convenient for 
  analysis.
}
\usage{RosterToLongDf(data, roster.base, id = "PUBID.1997")}
\arguments{
  \item{data}{a data frame}
  \item{roster.base}{the stable character prefix of the roster variable}
  \item{id}{character name representing the unit identifier}
}

\examples{
library(NLSdata)

\dontrun{
codebook <- system.file("Investigator", "Occupations.cdb", package = "NLSdata")
csv.extract <- system.file("Investigator", "Occupations.csv", package = "NLSdata")

nls.obj <- CreateNLSdata(codebook, csv.extract)
occ.df <- RosterToLongDf(nls.obj$data, "YEMP_OCCODE_2002")

occ.df$math.cs <- (occ.df$YEMP_OCCODE_2002 >= 1000 &
                   occ.df$YEMP_OCCODE_2002 <= 1240)
}
}
