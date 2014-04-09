\name{CreateTimeSeriesDf}
\alias{CreateTimeSeriesDf}
\title{Convert Repeated Element into Long Format Data Frame}
\description{the NLSdata package provides the function CreateTimeSeriesDf to coerce a portion of the data frame into a long format. Under the hood, it’s using the reshape2's melt function.}
\arguments{ 
  \item{obj}{an NLSdata object}
  \item{variable.base}{character string common prefix for the repeated element}
}
\examples{
library(NLSdata)
codebook <- system.file("Investigator", "Religion.cdb", package = "NLSdata")
csv.extract <- system.file("Investigator", "Religion.csv", package = "NLSdata")

nls.obj <- CreateNLSdata(codebook, csv.extract)

religion.df <- CreateTimeSeriesDf(nls.obj, "YSAQ_282A2")
head(religion.df[order(religion.df$PUBID.1997), ])
}

