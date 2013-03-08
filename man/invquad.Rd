\name{invquad}
\alias{invquad}
\title{Inverse Quadratic Roots}
\usage{
  invquad(a, b, c,y, roots="both", xmin=-Inf, xmax=Inf,
    na.rm=FALSE)
}
\arguments{
  \item{a}{The x^2 coefficient}

  \item{b}{The x coefficient}

  \item{c}{The Intercept}

  \item{y}{The y value at which the roots should be found}

  \item{roots}{To select which roots are required, "both"
  will give both, or "min" or "max"}

  \item{xmin}{The minimum X value to be considered}

  \item{xmax}{The maximum X value to be considered}

  \item{na.rm}{Should NA's be ignored}
}
\value{
  A Vector containing the calculated x values at the given
  y, Imaginary values given as NaN
}
\description{
  Calculate the inverse of a quadratic function y=ax^2+bx+c
  (ie find x when given y) Gives NaN with non real
  solutions.
}
\details{
  \code{invquad} is used to perform back caluclation of
  concentration values when performing quadratic
  regression.  The so-caled 'Regression Problem'
}
\examples{
invquad(1,1,-10,0)
invquad(0.1,0.01,-0.5,5:7,roots="max", xmin=1, xmax=15)
}
\author{
  Paul Hurley
}

