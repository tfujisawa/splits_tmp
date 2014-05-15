`spec.list`<-
function(res,second.peak=F) {
	tr <- res$tr
	spec <- tr$tip.label
	numtip <- length(tr$tip.label)
	
	if (second.peak==F) {
	max.mrca <- res$MRCA[[which.max(res$likelihood)]] + numtip} else
	{tmp<-table(cummax(res$likelihood))
		lik.peaks<-names(tmp[tmp>20])
		peak<-which(res$likelihood==lik.peaks[(length(lik.peaks)-1)])
		max.mrca <- res$MRCA[[peak]] + numtip}
	
	numspec <- length(max.mrca)
	
	nest.tip <- function(nod, tr) {
		
		tip <- c()
		child <- tr$edge[tr$edge[,1] == nod, 2]
		
		for (ch in child) {
			if (ch <= numtip) {
				tip <- c(tip, ch)
			} else {
				tip <- c(tip, nest.tip(ch, tr))
			}
		}
		return (tip)
	}
	
	res <- c()
	
	for (i in 1:length(max.mrca)) {
		tip.name <- tr$tip.label[nest.tip(max.mrca[i], tr)]
		res <- rbind(res, cbind(i, tip.name))	
	}
	
	
	if (length(spec[-match(res[,2], spec)]) != 0) {
		numspec <- numspec + 1
		for (s in spec[-match(res[,2], spec)]) {
			res <- rbind(res, cbind(numspec, s))
			numspec <- numspec + 1
		}
	}
	
	res <- data.frame(res)
	colnames(res) <- c("GMYC_spec", "sample_name")
	
	res$GMYC_spec <- as.numeric(as.character(res$GMYC_spec))
	
	return (res)
}
	