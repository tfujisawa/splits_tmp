`plot.gmyc` <-
function(res, ask=TRUE, second.peak=FALSE,file.name=NA,height=96) {
	#res = result of GMYC
	#plot results of GMYC analysis; likelihood, LTT plot and tree with clusters colored 
	
	if (ask) {
		par(ask=ask)
	} else {
		par(mfrow=c(3,1))
	}
	
	if (!is.na(file.name)) {pdf(file=paste(file.name,"ltt&lik.pdf",sep=""))}
	
	if (second.peak==TRUE) {
		tmp<-table(cummax(res$likelihood))
		lik.peaks<-names(tmp[tmp>20])
		peak<-which(res$likelihood==lik.peaks[(length(lik.peaks)-1)])}
	
	if (res[["method"]] == "single") {
		#lineage through time plot with threshold time
		ltt.plot(res$tree, log="y")
		if (second.peak==FALSE) {
		abline(v=res$threshold.time[which.max(res$likelihood)], col = "red")} else
		{abline(v=res$threshold.time[peak], col = "red")} 
		#likelihood surface
		plot(res$threshold.time, res$likelihood, type="l", xlab="Time", ylab="likelihood")
				
					if (!is.na(file.name)) {dev.off(); pdf(height=height,file=paste(file.name,"clust.pdf",sep=""))}
				
		if (second.peak==FALSE) {
		plot.cluster(res$tree, res$MRCA[[which.max(res$likelihood)]])} else
		{plot.cluster(res$tree, res$MRCA[[peak]])}
		
					if (!is.na(file.name)) {dev.off()}
	
	}  else if (res[["method"]] == "multiple" || res[["method"]] == "exhaustive") {
		#lineage through time plot with threshold time
		ltt.plot(res$tree, log="y")
		abline(v=res$threshold.time[[which.max(res$likelihood)]], col = "red")
		plot.cluster(res$tree, res$MRCA[[which.max(res$likelihood)]])
	# } else if (res[["method"]] == "multiple") {
		# plot.cluster(res$tree, res$MRCA[[which.max(res$likelihood)]])
	}
		
	#tree with clusters
	#plot.cluster1(res$tree, res$threshold.time[which.max(res$likelihood)])
	par(ask=FALSE)
}

