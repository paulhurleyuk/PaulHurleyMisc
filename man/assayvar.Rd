\name{assayvar}
\alias{assayvar}
\title{Apply Assay Variance function accross several nominal concentrations}
\usage{
  assayvar(adata, aresponse, afactor, anominal,
    aconfidence = 95, digits = 3, debug = FALSE)
}
\arguments{
  \item{adata}{The data frame to use, in long format}

  \item{afactor}{Character string representing the column
  in dataf that contains the factor}

  \item{aresponse}{Charactyer string representing the
  column in dataf that contains the response value}

  \item{anominal}{Character string representing the nominal
  conc column}

  \item{aconfidence}{What Confidence limits to use, default
  = 95%}

  \item{digits}{Significant Digits to report to, default =
  3}

  \item{debug}{Boolean, Should debug messages be displayed,
  default=FALSE}
}
\value{
  dataframe
}
\description{
  Apply Assay Variance function accross several nominal
  concentrations
}
\author{
  paul
}

