\name{avar}
\alias{avar}
\title{avar Function}
\usage{
  avar(dataf, afactor, aresponse, aconfidence = 95,
    digits = 3, debug = FALSE)
}
\arguments{
  \item{dataf}{- The data frame to use, in long format}

  \item{afactor}{Character string representing the column
  in dataf that contains the factor}

  \item{aresponse}{Charactyer string representing the
  column in dataf that contains the response value}

  \item{aconfidence}{What Confidence limits to use, default
  = 95%}

  \item{digits}{Significant Digits to report to, default =
  3}

  \item{debug}{Boolean, Should debug messages be displayed,
  default=FALSE}
}
\value{
  dataframe containing the Mean, Within, Between and Total
  %CV and LCB and UCB for each
}
\description{
  Calculate thewithin, between and total %CV of a dataset
  by ANOVA, and the associated confidence intervals
}
\examples{
#Using the balanced BGBottles data from Burdick and Graybill Page 62
avar(dataf=BGBottles, afactor="Machine", aresponse="weight")
#Using the unbalanced bull data from Burdick and Graybill Page 69
avar(dataf=bull, afactor="bull", aresponse="Percentage")
}
\author{
  Paul Hurley
}

