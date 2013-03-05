#' Insert linebreaks into long strings
#' @usage sapply(foo, pastwrap, len=22)
#' @param x A string
#' @param len Length to break at
#' @return string with linebreaks inserted every len characters
#' @author Paul Hurley
#' @export
pastewrap<-function(x, len=22) {
	return(paste(strwrap(x,width=len),sep="",collapse="\n"))
}
