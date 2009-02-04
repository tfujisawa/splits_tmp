`plot.gmyc` <-
function(x, ask=TRUE, second.peak=FALSE,file.name=NA,height=96, ...) {
	#res = result of GMYC
	#plot results of GMYC analysis; likelihood, LTT plot and tree with clusters colored 
	
	if (ask) {
		par(ask=ask)
	} else {
		par(mfrow=c(3,1))
	}
	
	if (!is.na(file.name)) {pdf(file=paste(file.name,"ltt&lik.pdf",sep=""))}
	
	if (second.peak==TRUE) {
		tmp<-table(cummax(x$likelihood))
		lik.peaks<-names(tmp[tmp>20])
		peak<-which(x$likelihood==lik.peaks[(length(lik.peaks)-1)])}
	
	if (x[["method"]] == "single") {
		#lineage through time plot with threshold time
		ltt.plot(x$tree, log="y")
		if (second.peak==FALSE) {
		abline(v=x$threshold.time[which.max(x$likelihood)], col = "red")} else
		{abline(v=x$threshold.time[peak], col = "red")} 
		#likelihood surface
		plot(x$threshold.time, x$likelihood, type="l", xlab="Time", ylab="likelihood")
				
					if (!is.na(file.name)) {dev.off(); pdf(height=height,file=paste(file.name,"clust.pdf",sep=""))}
				
		if (second.peak==FALSE) {
		plot.cluster(x$tree, x$MRCA[[which.max(x$likelihood)]])} else
		{plot.cluster(x$tree, x$MRCA[[peak]])}
		
					if (!is.na(file.name)) {dev.off()}
	
	}  else if (x[["method"]] == "multiple" || x[["method"]] == "exhaustive") {
		#lineage through time plot with threshold time
		ltt.plot(x$tree, log="y")
		abline(v=x$threshold.time[[which.max(x$likelihood)]], col = "red")
		plot.cluster(x$tree, x$MRCA[[which.max(x$likelihood)]])
	# } else if (x[["method"]] == "multiple") {
		# plot.cluster(x$tree, x$MRCA[[which.max(x$likelihood)]])
	}
		
	#tree with clusters
	#plot.cluster1(x$tree, x$threshold.time[which.max(x$likelihood)])
	par(ask=FALSE)
}

