`summary.gmyc` <-
function(res, second.peak=FALSE, ...) {
	#res = result of GMYC
	#display summary of GMYC; likelihood values, chi-square test, estimated parameters, etc...
	
		if (second.peak==TRUE) {
		tmp<-table(cummax(res$likelihood))
		lik.peaks<-names(tmp[tmp>20])
		peak<-which(res$likelihood==lik.peaks[(length(lik.peaks)-1)])}

	
	cat("Result of GMYC species delimitation\n")
	cat("\n\tmethod:\t", res[["method"]], sep="")
	cat("\n\tlikelihood of null model:\t", res$likelihood[1], sep="")
		if (second.peak==FALSE) {
			cat("\n\tmaximum likelihood of GMYC model:\t", max(res$likelihood), sep="")} else
			{cat("\n\tmaximum likelihood of GMYC model:\t", res$likelihood[peak], sep="")}
	
	#chisq test
	if (second.peak==FALSE) {
				LR <- 2*(max(res$likelihood)-res$likelihood[1])} else
				{LR <- 2*(res$likelihood[peak]-res$likelihood[1])}
	cat("\n\tlikelihood ratio:\t", LR, sep="")

	if (res[["method"]] == "single") {
		pvalue <- 1-pchisq(LR, 3)
	}  else if (res[["method"]] == "multiple" || res[["method"]] == "exhaustive") {
		pvalue <- 1 - pchisq(LR, 3 + length(res$threshold.time[[which.max(res$likelihood)]]) - 1)
	}
	
	cat("\n\tresult of LR test:\t", pvalue, ifelse(pvalue<0.001, "***", ifelse(pvalue<0.01, "**", ifelse(pvalue<0.05, "*", "n.s."))), sep="")
	
		if (second.peak==FALSE) {
	cat("\n\n\tnumber of ML clusters:\t", res$cluster[which.max(res$likelihood)], sep="")
		tmp<-res$cluster[res$likelihood>(max(res$likelihood)-2)]
		cat("\n\tconfidence interval:\t", paste(min(tmp),max(tmp),sep="-"), sep="")
	cat("\n\n\tnumber of ML entities:\t", res$entity[which.max(res$likelihood)], sep="")
		tmp<-res$entity[res$likelihood>(max(res$likelihood)-2)]
		cat("\n\tconfidence interval:\t", paste(min(tmp),max(tmp),sep="-"), sep="")

	if (res[["method"]] == "single") {	
		cat("\n\n\tthreshold time:\t", res$threshold.time[which.max(res$likelihood)], "\n", sep="")
	} else if (res[["method"]] == "multiple" || res[["method"]] == "exhaustive") {
		cat("\n\n\tthreshold time:\t", res$threshold.time[[which.max(res$likelihood)]], "\n", sep=" ")
	}	
	cat("\n")} else
	
	{cat("\n\n\tnumber of ML clusters:\t", res$cluster[peak], sep="")
	cat("\n\tnumber of ML entities:\t", res$entity[peak], sep="")
	if (res[["method"]] == "single") {	
		cat("\n\tthreshold time:\t", res$threshold.time[peak], "\n", sep="")
	} else if (res[["method"]] == "multiple" || res[["method"]] == "exhaustive") {
		cat("\n\tthreshold time:\t", res$threshold.time[[peak]], "\n", sep=" ")
	}	
	cat("\n")}
}

