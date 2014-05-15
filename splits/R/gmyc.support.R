`gmyc.support` <-
function(object, p=0.95) {
	object$MRCA[[1]] <- 1	#correct the index of root node....need to fix the GMYC function
	
	support <- rep(0, object$tree$Nnode)

	conf.model <- confset.gmyc(object, p)	
	
	mrca.conf <- conf.model$MRCA
	aw.conf <- conf.model$aw/sum(conf.model$aw)
	
	for (i in 1:length(mrca.conf)) {
		support[mrca.conf[[i]]] <- support[mrca.conf[[i]]] + aw.conf[i]
	}
	
	return (support)
}
