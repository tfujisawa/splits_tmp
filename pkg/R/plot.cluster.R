`plot.cluster` <-
function(tr, lthresh, show.tip.label=TRUE, show.node.label=FALSE, cex=0.5) {
	#tr = tree of ape tree class
	##thresh = a vector of threshold values
	
	numnod <- tr$Nnode
	numtip <- length(tr$tip.label)

	cdat<-array(1,2*numnod)
	ndat<-array("",numnod)
	
	bt <- -branching.times(tr)
	
	## Nested nodes
	nest.nodes<-function(tr, x ,p=0) {
		#print(paste("nest.nodes() called with x=", as.character(x)))
		numtip <- length(tr$tip.label)
		   
		nods<-array(NA,0)
		desc<-as.integer(tr$edge[,2][tr$edge[,1]==x])
		 
		if (desc[1] > numtip) {
			nods <- c(nods, desc[1], nest.nodes(tr, desc[1]))
		}
		if (desc[2] > numtip) {
			nods <- c(nods, desc[2], nest.nodes(tr, desc[2]))
		}
		   
		if (length(nods)>0) {
			return (nods) 
		} else {
			return(NULL)
		}      
	}
	
	threshold.group <- function(mrcas) {	#function to obtain multiple threshold times from a set of mrcas
		parent <- tr$edge[,1]
		child <- tr$edge[,2]
	
		thresh.group <- list()
		thresh.time <- c()
	
		mrcas <- mrcas + numtip
		k <- 1
		
		while (TRUE) {
			times <- bt[mrcas-numtip]
			thresh1.time <- min(times)
			thresh1.node <- mrcas[which.min(times)]
			
			mrcas <- mrcas[-which.min(times)]
			
			if (length(mrcas) == 0) { 
				thresh.time <- c(thresh.time, thresh1.time)	###??????????????????????? last MRCA ???
				thresh.group[[k]] <- thresh1.node
				break 
			}	
			
			member <- thresh1.node
			del <- c()
			for (i in 1:length(mrcas)) {
				#print(mrcas[i])
				#print(length(mrcas))
				par.nod <- parent[child==mrcas[i]]
				t.par <- bt[par.nod-numtip]
				#print(c(mrcas[i], par.nod, t.par, length(mrcas)))
				if (t.par < thresh1.time) {
						member <- c(member, mrcas[i])
						del <- c(del, i)
				}
				
			}
			thresh.time <- c(thresh.time, thresh1.time)
			thresh.group[[k]] <- member
			#print(thresh.time)
			k <- k+1
			
			if (length(del) != 0) {	mrcas <- mrcas[-del]}
			
			if (length(mrcas) == 0) { break }	
		}
		
		return (thresh.group)
	}
		
	group <- threshold.group(lthresh)
	

	#print(group)
	colors <- rainbow(length(group))
	
	k <- 1
	for (g in group) {
		n.col.type <- rep(0, numnod)
		for (j in 1:length(g)) {
			n.col.type[g[j]-numtip] <- 2
			n.col.type[nest.nodes(tr, g[j])-numtip] <- 1
			
		}
		cdat[match(tr$edge[,1],which((n.col.type==1)|(n.col.type==2))+numtip)>0]<-colors[k]
		#ndat[nod.type==2]<-(1:numnod)[n.col.type==2]
		k <- k + 1
	}
	
	
	plot(tr,edge.color=cdat, use.edge.length=1,show.node.label=show.node.label, show.tip.label=show.tip.label, no.margin=FALSE, cex=cex)
}
