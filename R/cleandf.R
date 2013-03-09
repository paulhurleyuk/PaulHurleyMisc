#' cleanf
#' 
#' Function to Strip repeat values from a df so it looks pretty in an Xtable
#' 
#' Thankx to Aniko 
#' (http://stackoverflow.com/questions/2379701/can-you-merge-cells-in-an-xtable-in-r/2380501#2380501
#' @param dataframe to be cleaned
#' @return Cleaned dataframe
#' @author "Paul Hurley"
#' @export
cleanf <- function(x){  
  oldx <- c(FALSE, x[-1]==x[-length(x)])  # is the value equal to the previous? 
  res <- x 
  res[oldx] <- NA         
  return(res)}

#' Function to Strip repeat rows from a df so it looks pretty in an Xtable
#' Thankx to Aniko 
#' (http://stackoverflow.com/questions/2379701/can-you-merge-cells-in-an-xtable-in-r/2380501#2380501
#' @param df dataframe to be cleaned
#' @param field Name of column to find duplicates with
#' @return Cleaned dataframe
#' @author "Paul Hurley"
#' @export
cleandf <- function(df, field){  
	if(length(field)>1){
		rowtoremove <- c(FALSE, apply(df[-1,field]==df[-nrow(df),field], 
						MARGIN=1, FUN=all))  
		# is the value equal to the previous? 
	} else {
		rowtoremove <- c(FALSE, df[-1,field]==df[-nrow(df),field])  
		# is the value equal to the previous? 
	}
	res <- df[!rowtoremove,]    
	return(res)} 

