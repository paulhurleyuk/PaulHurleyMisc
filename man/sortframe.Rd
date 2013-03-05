\name{sortframe}
\alias{sortframe}
\title{Function to Sort a dataframe with a given list of columns
Cribbed from Spector, P. (2008). "Data Manipulation with R", UseR! Springer. Pg78}
\usage{
  with(dataframe,sortframe(dataframe,column1, column2,
    column3))
}
\arguments{
  \item{df}{Dataframe to be sorted}

  \item{...}{list of columns to sort on}
}
\value{
  A sorted dataframe
}
\description{
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

