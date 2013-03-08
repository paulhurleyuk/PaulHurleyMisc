#' Calculate co-efficient of variation
#' 
#' @param x a value 
#' @param na.rm - ignore na's
#' @return numeric Return the CV
#' 
#' @author Paul Hurley
#' @export
cv<-function(x, na.rm=FALSE){
  cv<-(100/mean(x, na.rm=na.rm))*(sd(x, na.rm=na.rm))
  return(cv)
}

#' Calculate the Percentage Bias of a dataframe
#' 
#' @param x a value 
#' @param nominal the nominal value
#' @return numeric Return the percentage bias of x in terms of nominal
#' 
#' @author Paul Hurley
#' @export
pBias<-function(x, nominal, na.rm=FALSE){
  pBias<-(mean(x, na.rm=na.rm)-nominal)/nominal*100
  return(pBias)
}

#' Lund Outlier Tests
#' 
#' Calculates a Critical value / probability for Outlier Test according to Lund
#' 
#' Calculates a critical value to compare against the studentised residual for 
#' outlier testing in a polynomial regression.  \code{lundprob} calculates the probability
#' of a given studentisted residual being an outlier by this method.
#' 
#' Generally this method is used to identify (and eliminate) a single value.  If required
#' the regression is updated and the values re-calculated.
#' 
#' @references 
#' Richard E Lund. Tables for an approximate test for outliers in linear models.
#'  Technometrics,17(4):473-476, 1975.
#' P Prescott. An approximate test for outliers in linear models. Technometrics, 
#' 17(1):129-132,1975.
#' 
#' @param a Chosen alpha value
#' @param n Number of Data Elements
#' @param q Number of Independent Variables (including Intercept)
#' @return The Critical Value
#' @author Paul Hurley
#' @aliases lundprob
#' @export
#' @examples
#' data(phosphorus)
#' phoslm<-lm(Y~X1+X2,data=phosphorus)
#' summary(phoslm)
#' phosphorus$fitted<-fitted(phoslm)
#' phosphorus$residual<-residuals(phoslm)
#' phosphorus$standardresid<-rstandard(phoslm)
#' n<-nrow(phosphorus)
#' q<-length(phoslm$coefficients)
#' crit<-lundcrit(0.1,n,q)
#' phosphorus$Ynew<-ifelse(abs(phosphorus$standardresid)>crit,NA,phosphorus$Y)
#' print(phosphorus)
lundcrit<-function(a, n, q) {
  # Calculates a Critical value for Outlier Test according to Lund
  #
  # a = alpha
  # n = Number of data elements
  # q = Number of independent Variables (including intercept)
  # Was + F<-F<-qf(c(1-(a/n)),df1=1,df2=n-q-1,lower.tail=TRUE) but removed the 
  #	dupe F<-, PH 28 aug 2009
  F<-qf(c(1-(a/n)),df1=1,df2=n-q-1,lower.tail=TRUE)
  crit<-((n-q)*F/(n-q-1+F))^0.5
  return(crit)
}

#' @export
lundprob<-function(crit, n, q){
  # Calculates an alpha value for a matchinv studantized residual
  #
  # crit = studantized residual
  # n = Number of data elements
  # q = Number of independent Variables (including intercept)
  F<-((n*crit^2)-(q*crit^2)-crit^2)/(n-q-crit^2)	
  prob<-pf(c(F), df1=1, df2=n-q-1, lower.tail=TRUE)
  alpha<-(1-prob)*n
  return(alpha)	
}
