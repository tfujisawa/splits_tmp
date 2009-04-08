dimReduct <-
function (XX, how="PA", scale=TRUE, iterations, centile) 
	{
	if (!is.matrix(XX)) XX <- as.matrix(XX)
	if (how!="KG" & how!="PA") stop ("Invalid method to reduce dimensions.\nUse either "KG" or "PA".")
	
	m1<-prcomp(XX,scale=scale)
	if (how=="KG") {nk<-max(which(m1$sdev^2>=1))}
	if (how=="PA") {nk<- as.numeric(paran(XX, iterations=iterations,centile=centile,quietly=TRUE)[[1]])}
	dR <- sS=predict(m1)[,1:nk]
	return(dR)
	}

