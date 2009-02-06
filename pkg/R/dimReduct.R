`dimReduct` <-
function (XX, how="PA", scale=TRUE) 
	{
	if (!is.matrix(XX)) stop("XX is not a matrix.")
	if (how!="KG" & how!="PA") stop ("Invalid method to reduce dimensions.")
	
	m1<-prcomp(XX,scale=scale)
	if (how=="KG") {nk<-as.numeric(nScree(m1$sdev^2)$Components[4])}
	if (how=="PA") {nk<- as.numeric(paran(XX, iterations=2000,centile=95,quietly=TRUE)[[1]])}
	dR <- list(strippedSpace=predict(m1)[,1:nk], nF=nk)
	return(dR)
	}

