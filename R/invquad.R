# TODO: Add comment
# 
# Author: gb02413
###############################################################################


#' Inverse Quadratic Roots
#' 
#' Calculate the inverse of a quadratic function y=ax^2+bx+c (ie find x when given y)
#' Gives NaN with non real solutions.
#' 
#' \code{invquad} is used to perform back caluclation of concentration values when 
#' performing quadratic regression.  The so-caled 'Regression Problem'
#' 
#' @usage invquad(a, b, c,y, roots="both", xmin=-Inf, xmax=Inf, na.rm=FALSE)
#' 
#' @param a The x^2 coefficient
#' @param b The x coefficient
#' @param c The Intercept
#' @param y The y value at which the roots should be found
#' @param roots To select which roots are required, "both" will give both, or "min" or "max"
#' @param xmin The minimum X value to be considered
#' @param xmax The maximum X value to be considered
#' @param na.rm Should NA's be ignored
#' @return A Vector containing the calculated x values at the given y, Imaginary values given as NaN
#' @author Paul Hurley
#' @export
#' @examples 
#' invquad(1,1,-10,0)
#' invquad(0.1,0.01,-0.5,5:7,roots="max", xmin=1, xmax=15)
invquad<-function(a,b,c,y,roots="both", xmin=(-Inf), xmax=(Inf),na.rm=FALSE){
	root1<-sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))
	root2<--sqrt((y-(c-b^2/(4*a)))/a)-(b/(2*a))
	root1<-ifelse(root1<xmin,NA,root1)	
	root1<-ifelse(root1>xmax,NA,root1)	
	root2<-ifelse(root2<xmin,NA,root2)	
	root2<-ifelse(root2>xmax,NA,root2)
	if (roots=="both") {	
		result<-c(root1,root2)
		if (na.rm) result<-ifelse(is.na(root1),root2, result)
		if (na.rm) result<-ifelse(is.na(root2),root1,result)
		if (na.rm) result<-ifelse(is.na(root1)&is.na(root2),NA,result)
	}
	if (roots=="min")
		result<-pmin(root1,root2, na.rm=TRUE)
	if (roots=="max")
		result<-pmax(root1,root2, na.rm=TRUE)
	return(result)
}
