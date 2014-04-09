\name{RosterToLongDf}
\alias{RosterToLongDf}
\title{Conversion of Roster Variables to Long Format}
\usage{RosterToLongDf(data, roster.base, id = "PUBID.1997")}
\arguments{
  \item{data}{a data frame}
  \item{roster.base}{the stable character prefix of the roster variable}
  \item{id}{character name representing the unit identifier}
}

\examples{
library(NLSdata)
codebook <- system.file("Investigator", "Occupations.cdb", package = "NLSdata")
csv.extract <- system.file("Investigator", "Occupations.csv", package = "NLSdata")

nls.obj <- CreateNLSdata(codebook, csv.extract)
occ.df <- RosterToLongDf(nls.obj$data, "YEMP_OCCODE_2002")

occ.df$math.cs <- (occ.df$YEMP <- OCCODE_2002 >= 1000 &
                   occ.df$YEMP <- OCCODE_2002 <= 1240)
}
