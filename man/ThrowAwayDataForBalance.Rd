\name{ThrowAwayDataForBalance}
\alias{ThrowAwayDataForBalance}
\title{Achieve Balance over Time Period and Respondent by Crude Subsetting}
\usage{ThrowAwayDataForBalance}
\arguments{
\item{data}{a data frame}
\item{var.name}{variable to achieve balance with respect to}
\item{id}{id variable, also to achieve balance with respect to}
}
\details{Statistically this is a very crude method, with potential bias from a response
         driven sampling method. It's meant for quick-and-dirty analyses of a single
         variable over time and until more sophisticated missing data procedures are
         integrated with NLSdata.}

\examples{
  cdebook <- system.file("Investigator", "Religion.cdb", package = "NLSdata")
  csv.extract <- system.file("Investigator", "Religion.csv", package = "NLSdata")

  nls.obj <- CreateNLSdata(codebook, csv.extract)

  # Since only two people answered the question that year, excluding it from the analysis
  religion.df <- religion.df[religion.df$year != 2006, ]

  religion.df <- ThrowAwayDataForBalance(religion.df, "YSAQ_282A2")
  table(religion.df$year)
  head(table(religion.df$PUBID.1997))

}

