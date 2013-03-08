\name{sortframe}
\alias{sortframe}
\title{Sort a dataframe}
\usage{
  with(df,sortframe(df,column1, column2, column3))
}
\arguments{
  \item{df}{Dataframe to be sorted}

  \item{...}{list of columns to sort on}
}
\value{
  A sorted dataframe
}
\description{
  Orders based on input fields, used with \code{with} will
  order a df
}
\details{
  Function to Sort a dataframe with a given list of columns
  Cribbed from Spector, P. (2008). "Data Manipulation with
  R", UseR! Springer. Pg78
}
\examples{
with(iris,sortframe(iris,Sepal.Length,Sepal.Width,Petal.Length))
}
\author{
  "Paul Hurley"
}

