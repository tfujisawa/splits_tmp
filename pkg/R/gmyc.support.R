`gmyc.support` <-
function(object, p=0.95) {
	#obtain Akaike weight for each hypothesis
	#object: gmyc object
	#returns Akaike weights of models in gmyc object
	akaike.weght <- function(object) {
		lik <- object$likelihood
		names(lik) <- 1:length(lik)

		aic <- -2*lik + 2*c(2, rep(4, length(lik)-1))
		dif <- aic - min(aic)
		weight <- exp(-dif/2)/sum(exp(-dif/2))

		return (weight)
	}	

	#obtain index  of MRCAs included in p confidence set
	#object: gmyc object
	#p: confidence value
	#returns index of models included in p% confidence set
	conf.set <- function(object, p=0.95) {
		weight <- akaike.weght(object)
		
		names(weight) <- 1:length(weight)
		
		sw <- sort(weight, decreasing=T)
		csw <- cumsum(sw)
		
		if (csw[1] >= p) {
			confset <- as.numeric(names(csw[1]))
		} else {
			confset <- as.numeric(names(csw[csw <= p]))
		}
		
		return (confset)
	}


	object$MRCA[[1]] <- 1	#correct the index of root node....need to fix the GMYC function
	
	support <- rep(0, object$tree$Nnode)
	aw <- akaike.weght(object)
	
	id.conf <- conf.set(object, p)
	
	mrca.conf <- object$MRCA[id.conf]
	aw.conf <- aw[id.conf]/sum(aw[id.conf])
	
	for (i in 1:length(mrca.conf)) {
		support[mrca.conf[[i]]] <- support[mrca.conf[[i]]] + aw.conf[i]
	}
	
	return (support)
}
