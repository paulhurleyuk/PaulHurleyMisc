\name{pastewrap}
\alias{pastewrap}
\title{Insert linebreaks into long strings}
\usage{
  pastewrap(x, len=22)
}
\arguments{
  \item{x}{A string}

  \item{len}{Length to break at}
}
\value{
  string with linebreaks inserted every len characters
}
\description{
  Inserts liebreaks into long strings at specified
  intervals
}
\examples{
pastewrap("a very long string that needs to be wrapped", len=12)
sapply(c("a vector of very long","strings that need to", "be wrapped"), pastewrap, len=10)
}
\author{
  Paul Hurley
}

