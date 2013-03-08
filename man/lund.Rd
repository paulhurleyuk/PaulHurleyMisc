\name{lundcrit}
\alias{lundcrit}
\alias{lundprob}
\title{Lund Outlier Tests}
\usage{
  lundcrit(a, n, q)

  lundprob(crit, n, q)
}
\arguments{
  \item{a}{Chosen alpha value}

  \item{n}{Number of Data Elements}

  \item{q}{Number of Independent Variables (including
  Intercept)}
}
\value{
  The Critical Value
}
\description{
  Calculates a Critical value / probability for Outlier
  Test according to Lund
}
\details{
  Calculates a critical value to compare against the
  studentised residual for outlier testing in a polynomial
  regression.  \code{lundprob} calculates the probability
  of a given studentisted residual being an outlier by this
  method.

  Generally this method is used to identify (and eliminate)
  a single value.  If required the regression is updated
  and the values re-calculated.
}
\examples{
data(phosphorus)
phoslm<-lm(Y~X1+X2,data=phosphorus)
summary(phoslm)
phosphorus$fitted<-fitted(phoslm)
phosphorus$residual<-residuals(phoslm)
phosphorus$standardresid<-rstandard(phoslm)
n<-nrow(phosphorus)
q<-length(phoslm$coefficients)
crit<-lundcrit(0.1,n,q)
phosphorus$Ynew<-ifelse(abs(phosphorus$standardresid)>crit,NA,phosphorus$Y)
print(phosphorus)
}
\author{
  Paul Hurley
}
\references{
  Richard E Lund. Tables for an approximate test for
  outliers in linear models.  Technometrics,17(4):473-476,
  1975. P Prescott. An approximate test for outliers in
  linear models. Technometrics, 17(1):129-132,1975.
}

