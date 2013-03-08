#' Improved Significant Figures Function
#' 
#' @param x Vector of numeric values 
#' @param digits Number of significant digits to display
#' @return Text string of the value to the correct sig figs
#' @author Paul Hurley
#' @export
fsignif<-function(x, digits=3){
  return(formatC(signif(x, digits=digits), digits=digits,format="fg", 
                 flag="#"))
}
