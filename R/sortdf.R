#' Sort a dataframe
#' 
#' Orders based on input fields, used with \code{with} will order a df
#' 
#' Function to Sort a dataframe with a given list of columns
#' Cribbed from Spector, P. (2008). "Data Manipulation with R", UseR! Springer. Pg78
#' 
#' @usage with(df,sortframe(df,column1, column2, column3))
#' 
#' @param df Dataframe to be sorted
#' @param ... list of columns to sort on
#' @return A sorted dataframe
#' @author "Paul Hurley"
#' @export
#' 
#' @examples with(iris,sortframe(iris,Sepal.Length,Sepal.Width,Petal.Length))
sortframe<-function(df,...){df[do.call(order,list(...)),]}

