#' Insert linebreaks into long strings
#' 
#' Inserts liebreaks into long strings at specified intervals
#' 
#' @usage pastewrap(x, len=22)
#' 
#' @param x A string
#' @param len Length to break at
#' @return string with linebreaks inserted every len characters
#' @author Paul Hurley
#' @export
#' @examples
#' pastewrap("a very long string that needs to be wrapped", len=12)
#' sapply(c("a vector of very long","strings that need to", "be wrapped"), pastewrap, len=10)
pastewrap<-function(x, len=22) {
	return(paste(strwrap(x,width=len),sep="",collapse="\n"))
}
