`confset.gmyc` <-  
function(object, p=0.95) {
	akaike.weight <- function(object) {
		lik <- object$likelihood
		names(lik) <- 1:length(lik)

		aic <- -2*lik + 2*c(2, rep(4, length(lik)-1))
		dif <- aic - min(aic)
		weight <- exp(-dif/2)/sum(exp(-dif/2))

		return (weight)
	}	


	weight <- akaike.weight(object)
	
	names(weight) <- 1:length(weight)
	
	sw <- sort(weight, decreasing=T)
	csw <- cumsum(sw)
	
	if (csw[1] >= p) {
		confset <- as.numeric(names(csw[1]))
	} else {
		confset <- as.numeric(names(csw[csw <= p]))
	}
	
	object[["likelihood"]] <- object[["likelihood"]][confset]
	object[["parameters"]] <- object[["parameters"]][confset,]
	object[["entity"]] <- object[["entity"]][confset]
	object[["cluster"]] <- object[["cluster"]][confset]
	object[["MRCA"]] <- object[["MRCA"]][confset]
	object[["threshold.time"]] <- object[["threhold.time"]][confset]
	object[["aw"]] <- weight[confset]
	object[["method"]] <- "conf"
	
	return (object)
}

