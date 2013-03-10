# TODO: Add comment
# 
# Author: Paul hurley
###############################################################################


#' avar Function
#' 
#' Calculate thewithin, between and total %CV of a dataset by ANOVA, and the
#' associated confidence intervals
#' 
#' @param dataf - The data frame to use, in long format 
#' @param afactor Character string representing the column in dataf that contains the factor
#' @param aresponse  Charactyer string representing the column in dataf that contains the response value
#' @param aconfidence What Confidence limits to use, default = 95%
#' @param digits  Significant Digits to report to, default = 3
#' @param debug Boolean, Should debug messages be displayed, default=FALSE
#' @return dataframe containing the Mean, Within, Between and Total %CV and LCB and UCB for each
#' @author Paul Hurley
#' @export
#' @examples 
#' #Using the balanced BGBottles data from Burdick and Graybill Page 62
#' avar(dataf=BGBottles, afactor="Machine", aresponse="weight")
#' #Using the unbalanced bull data from Burdick and Graybill Page 69
#' avar(dataf=bull, afactor="bull", aresponse="Percentage")
avar<-function(dataf, afactor, aresponse, aconfidence=95, digits=3, debug=FALSE){
	dataf<-subset(dataf,!is.na(with(dataf,get(aresponse))))
	nmissing<-function(x) sum(!is.na(x))
	n<-nrow(subset(dataf,is.numeric(with(dataf,get(aresponse)))))
	datadesc<-ddply(dataf, afactor, colwise(nmissing,aresponse))
	I<-nrow(datadesc)
	if(debug){print(datadesc)}
	if(min(datadesc[,2])==max(datadesc[,2])){
		balance<-TRUE
		J<-min(datadesc[,2])
		if(debug){message(paste("Dataset is balanced, J=",J,"I is ",I,sep=""))}
	} else {
		balance<-FALSE
		Jh<-I/(sum(1/datadesc[,2], na.rm = TRUE))
		J<-Jh
		m<-min(datadesc[,2])
		M<-max(datadesc[,2])
		if(debug){message(paste("Dataset is unbalanced, like me, I is ",I,sep=""))}
		if(debug){message(paste("Jh is ",Jh, ", m is ",m, ", M is ",M, sep=""))}
	}
	if(debug){message(paste("Call afactor=",afactor,", aresponse=",aresponse,sep=""))}
	formulatext<-paste(as.character(aresponse)," ~ 1 + Error(",as.character(afactor),")",sep="")
	if(debug){message(paste("formula text is ",formulatext,sep=""))}
	aovformula<-formula(formulatext)
	if(debug){message(paste("Formula is ",as.character(aovformula),sep=""))}
	assayaov<-aov(formula=aovformula,data=dataf)
	if(debug){
		print(assayaov)
		print(summary(assayaov))
	}
	a<-1-((1-(aconfidence/100))/2)
	if(debug){message(paste("confidence is ",aconfidence,", alpha is ",a,sep=""))}
	grandmean<-as.vector(assayaov$"(Intercept)"[[1]][1]) # Grand Mean (I think)
	if(debug){message(paste("n is",n,sep=""))}
	
	#This line commented out, seems to choke with an aov object built from an external formula
	#grandmean<-as.vector(model.tables(assayaov,type="means")[[1]]$`Grand mean`) # Grand Mean (I think)
	within<-summary(assayaov)[[2]][[1]]$"Mean Sq"  # d2e, S2^2 Mean Square Value for Within Machine = 0.1819
	dfRun<-summary(assayaov)[[1]][[1]]$"Df"  # DF for within = 3
	dfWithin<-summary(assayaov)[[2]][[1]]$"Df"  # DF for within = 8
	Run<-summary(assayaov)[[1]][[1]]$"Mean Sq" # S1^2Mean Square for Machine
	if(debug){message(paste("mean square for Run ?",Run,sep=""))}
	#Was between<-(Run-within)/((dfWithin/(dfRun+1))+1) but my comment suggests this should be just J, so I'll use J !
	between<-(Run-within)/J # d2a (S1^2-S2^2)/J
	if(debug){message(paste("S1^2 mean square machine is ",Run,", S2^2 mean square within is ",within))}
	total<-between+within
	between # Between Run Variance
	within # Within Run Variance
	total # Total Variance
	if(debug){message(paste("between is ",between,", within is ",within,", Total is ",total,sep=""))}
	
	betweenCV<-sqrt(between)/grandmean * 100 # Between Run CV%
	withinCV<-sqrt(within)/grandmean * 100 # Within Run CV%
	totalCV<-sqrt(total)/grandmean * 100 # Total CV%
	n1<-dfRun
	n2<-dfWithin
	if(debug){message(paste("n1 is ",n1,", n2 is ",n2,sep=""))}
	#within confidence intervals
	if(balance){
		withinLCB<-within/qf(a,n2,Inf) # Within LCB
		withinUCB<-within/qf(1-a,n2,Inf) # Within UCB
	} else {
		withinLCB<-within/qf(a,n2,Inf) # Within LCB
		withinUCB<-within/qf(1-a,n2,Inf) # Within UCB
	}
#Mean Confidence Intervals
	if(debug){message(paste(grandmean,"+/-(sqrt(",Run,"/",n,")*qt(",a,",df=",I-1,"))",sep=""))}	
	meanLCB<-grandmean+(sqrt(Run/n)*qt(1-a,df=I-1)) # wrong
	meanUCB<-grandmean-(sqrt(Run/n)*qt(1-a,df=I-1)) # wrong
	if(debug){message(paste("Grandmean is ",grandmean,", meanLCB = ",meanLCB,", meanUCB = ",meanUCB,aresponse,sep=""))}
	if(debug){print(summary(assayaov))}
#Between Confidence Intervals
	G1<-1-(1/qf(a,n1,Inf)) 
	G2<-1-(1/qf(a,n2,Inf))
	H1<-(1/qf(1-a,n1,Inf))-1  
	H2<-(1/qf(1-a,n2,Inf))-1
	G12<-((qf(a,n1,n2)-1)^2-(G1^2*qf(a,n1,n2)^2)-(H2^2))/qf(a,n1,n2) 
	H12<-((1-qf(1-a,n1,n2))^2-H1^2*qf(1-a,n1,n2)^2-G2^2)/qf(1-a,n1,n2) 
	if(debug){message(paste("G1 is ",G1,", G2 is ",G2,sep=""))
		message(paste("H1 is ",H1,", H2 is ",H2,sep=""))
		message(paste("G12 is ",G12,", H12 is ",H12,sep=""))
	}
	if(balance){
		Vu<-H1^2*Run^2+G2^2*within^2+H12*Run*within
		Vl<-G1^2*Run^2+H2^2*within^2+G12*within*Run
		betweenLCB<-(Run-within-sqrt(Vl))/J # Betwen LCB
		betweenUCB<-(Run-within+sqrt(Vu))/J # Between UCB
	} else {
		#Burdick and Graybill seem to suggest calculating anova of mean values to find n1S12u/Jh
		meandataf<-ddply(.data=dataf,.variable=afactor, .fun=function(df){mean(with(df, get(aresponse)), na.rm=TRUE)})
		meandataaov<-aov(formula(paste("V1~",afactor,sep="")), data=meandataf)
		sumsquare<-summary(meandataaov)[[1]]$`Sum Sq`
		#so maybe S12u is just that bit ?
		Runu<-(sumsquare*Jh)/n1
		if(debug){message(paste("n1S12u/Jh is ",sumsquare,", so S12u is ",Runu,sep=""))}
		Vu<-H1^2*Runu^2+G2^2*within^2+H12*Runu*within
		Vl<-G1^2*Runu^2+H2^2*within^2+G12*within*Runu
		betweenLCB<-(Runu-within-sqrt(Vl))/Jh # Betwen LCB
		betweenUCB<-(Runu-within+sqrt(Vu))/Jh # Between UCB
		if(debug){message(paste("betweenLCB is ",betweenLCB,", between UCB is ",betweenUCB,sep=""))}
	}
#Total Confidence Intervals
	if(balance){
		y<-(Run+(J-1)*within)/J
		if(debug){message(paste("y is ",y,sep=""))}
		totalLCB<-y-(sqrt(G1^2*Run^2+G2^2*(J-1)^2*within^2)/J) # Total LCB
		totalUCB<-y+(sqrt(H1^2*Run^2+H2^2*(J-1)^2*within^2)/J) # Total UCB
	} else {
		y<-(Runu+(Jh-1)*within)/Jh
		if(debug){message(paste("y is ",y,sep=""))}
		totalLCB<-y-(sqrt(G1^2*Runu^2+G2^2*(Jh-1)^2*within^2)/Jh) # Total LCB
		totalUCB<-y+(sqrt(H1^2*Runu^2+H2^2*(Jh-1)^2*within^2)/Jh) # Total UCB
	}
	if(debug){message(paste("totalLCB is ",totalLCB,", total UCB is ",totalUCB,sep=""))}
#	result<-data.frame(Name=c("within", "between", "total"),CV=c(withinCV,betweenCV,totalCV),
#			LCB=c(sqrt(withinLCB)/grandmean*100,sqrt(betweenLCB)/grandmean*100,sqrt(totalLCB)/grandmean*100),
#			UCB=c(sqrt(withinUCB)/grandmean*100,sqrt(betweenUCB)/grandmean*100,sqrt(totalUCB)/grandmean*100))
	result<-data.frame(Mean=grandmean,MeanLCB=meanLCB, MeanUCB=meanUCB, Within=withinCV,WithinLCB=sqrt(withinLCB)/grandmean*100, WithinUCB=sqrt(withinUCB)/grandmean*100,
			Between=betweenCV, BetweenLCB=sqrt(betweenLCB)/grandmean*100, BetweenUCB=sqrt(betweenUCB)/grandmean*100,
			Total=totalCV, TotalLCB=sqrt(totalLCB)/grandmean*100, TotalUCB=sqrt(totalUCB)/grandmean*100)
	if(!digits=="NA"){
		result$Mean<-signif(result$Mean,digits=digits)
		result$MeanLCB<-signif(result$MeanLCB,digits=digits)
		result$MeanUCB<-signif(result$MeanUCB,digits=digits)
		result$Within<-signif(result$Within,digits=digits)
		result$WithinLCB<-signif(result$WithinLCB,digits=digits)
		result$WithinUCB<-signif(result$WithinUCB,digits=digits)
		result$Between<-signif(result$Between,digits=digits)
		result$BetweenLCB<-signif(result$BetweenLCB,digits=digits)
		result$BetweenUCB<-signif(result$BetweenUCB,digits=digits)
		result$Total<-signif(result$Total,digits=digits)
		result$TotalLCB<-signif(result$TotalLCB,digits=digits)
		result$TotalUCB<-signif(result$TotalUCB,digits=digits)
	}
	return(result)
}

#' Apply Assay Variance function accross several nominal concentrations
#' 
#' @param adata The data frame to use, in long format 
#' @param afactor Character string representing the column in dataf that contains the factor
#' @param aresponse  Charactyer string representing the column in dataf that contains the response value
#' @param anominal Character string representing the nominal conc column
#' @param aconfidence What Confidence limits to use, default = 95%
#' @param digits  Significant Digits to report to, default = 3
#' @param debug Boolean, Should debug messages be displayed, default=FALSE
#' @return dataframe
#' 
#' @author paul
#' @export
assayvar<-function(adata, aresponse, afactor, anominal, aconfidence=95, digits=3, debug=FALSE){
	result<-ddply(adata,anominal,function(df){
				resul<-avar(dataf=df,afactor=afactor,aresponse=aresponse,aconfidence=aconfidence, digits=digits, debug=debug)
				resul$n<-nrow(subset(df, !is.na(with(df, get(aresponse)))))
				return(resul)
			})
	return(result)
}