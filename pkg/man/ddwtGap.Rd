\name{ddwtGap}
\alias{ddwtGap}
\alias{getWbarK}
\alias{genRefdata}
\title{Identify the number of clusters in a data matrix.}
\description{
Calculate the number of clusters using adapted versions of the weighted-gap (wtGap, Tibshirani et al., 2001) and double-difference weighted-gap (DDwtGap, Yan & Ye, 2007) algorithms.  Two clustering methods are implemented: kmeans and hierarchical clustering.  See \code{details} below.
}
\usage{
ddwtGap(XX, maxClust, nTruth = 20, nRep = 100, status = TRUE, method = "hc", genRndm = "pc")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{A matrix of measurements.  Each row corresponds to one observation.}
  \item{maxClust}{The maximum number of clusters to calculate the statistics for.  Note that the DD weighted gap only returns values between 2 and (maxClust-1) clusters.}
  \item{nTruth}{Number of times to repeat procedures with different seeds used to calculate the *truth*.}
  \item{nRep}{The number of reference data sets used to calculate standard errors around the weighted-gap statistic.}
  \item{status}{Should progress updates be returned?  Defaults to TRUE.}
  \item{method}{Which clustering method should be employed?  Defaults to "hc" for hierarchical clustering; use "km" for kmeans.}
  \item{genRndm}{How should the random reference data be generated?  Defaults to "pc" for principal components, which simulates data that approximates the distribution of the original data in XX; use "uni" for uniform random data across the range of observations in XX.}
}
\details{
kmeans biased towards circular clusters; hc based on cutting a dendrogram.

nTruth.  In an ideal world, allGhat below shows no variance.

genRefdata is an internal function that simulates data sets to obtain standard errors for wGap statistics from.  This is done depending on how \code{genRndm} is specified.
}

\value{
A list containing:
  \item{mnGhatWG} {The mean number of well-separated clusters based on wGap.}
  \item{allGhatWG}{all Ghats: the number of well-separated clusters from each simulation/for each *truth* based on wGap.}
  \item{mnGhatDD} {The mean number of well-separated clusters based on DDwGap.}
  \item{allGhatDD}{all Ghats: the number of well-separated clusters from each simulation/for each *truth* based on DDwGap.}
  \item{wGap}{A matrix of weighted-gap statistics (Tibshirani et al., 2001) for each *truth* and number of clusters.}
  \item{seGap}{The standard errors associated with each wGap entry.}
  \item{DDwGap}The double-difference weighted-gap statistic (Yan & Ye, 2007) for each *truth* and number of clusters.}

\references{ ~put references to the literature/web site here ~ }
\author{Thomas H.G. Ezard \email{thomas.ezard@imperial.ac.uk}}
\seealso{\code{\link{dimReduct}}, \code{\link{subsDiag}}}
\examples{
#############################################################
##two well separated clusters
##GENERATE RANDOM DATA
x1  <- rnorm(50,0.5,0.2)  ;  y1 <- rnorm(50,mean=0.5,sd=0.2)
x2  <- rnorm(50,1.5,0.2)  ;  y2 <- rnorm(50,mean=1.5,sd=0.2)
sp1 <- matrix(c(x1,x2,y1,y2),ncol=2,byrow=FALSE)
##PLOT DATA
par(mfrow=c(3,1))
plot(sp1,pch=c(rep(1,100),rep(16,100)))

##IDENTIFY CLUSTERS
#eg1 <- ddwtGap(sp1,10,nRep=50)
##PLOT WEIGHTED-GAP AND DD WEIGHTED-GAP
#plot(colMeans(eg1$wGap),pch=15,type='b',xlab="Number of Clusters", ylab="wtGap",ylim=extendrange(colMeans(eg1$wGap),f=0.2))

#arrows(1:dim(eg1$wGap)[2],colMeans(eg1$wGap)-colMeans(eg1$seGap), 1:dim(eg1$wGap)[2], colMeans(eg1$wGap)+colMeans(eg1$seGap),length=0.05,angle=90,code=3)
#plot(colMeans(eg1$DDwGap),pch=15,type='b',xlab="Number of Clusters", ylab="DDwtGap",ylim=extendrange(colMeans(eg1$DDwGap),f=0.2))

#############################################################
##two partially overlapping clusters, no guarantee that they are well-separated
##GENERATE RANDOM DATA
x1  <- rnorm(50,0.5,0.4)  ;  y1 <- rnorm(50,mean=0.5,sd=0.4)
x2  <- rnorm(50,1.5,0.4)  ;  y2 <- rnorm(50,mean=1.5,sd=0.4)
sp2 <- matrix(c(x1,x2,y1,y2),ncol=2,byrow=FALSE)
##PLOT DATA
par(mfrow=c(3,1))
plot(sp2,pch=c(rep(1,100),rep(16,100)))

##IDENTIFY CLUSTERS
#eg2 <- ddwtGap(sp2,10,nRep=50)
##PLOT WEIGHTED-GAP AND DD WEIGHTED-GAP
#plot(colMeans(eg2$wGap),pch=15,type='b',xlab="Number of Clusters", ylab="wtGap",ylim=extendrange(colMeans(eg2$wGap),f=0.2))#arrows(1:dim(eg2$wGap)[2],colMeans(eg2$wGap)-colMeans(eg2$seGap), 1:dim(eg2$wGap)[2],colMeans(eg2$wGap)+colMeans(eg2$seGap),length=0.05,angle=90,code=3)
#plot(colMeans(eg2$DDwGap),pch=15,type='b',xlab="Number of Clusters", ylab="DDwtGap",ylim=extendrange(colMeans(eg2$DDwGap),f=0.2))

#############################################################
##two partially overlapping clusters, *always* struggles to statistically distinguish them
##GENERATE RANDOM DATA
x1  <- rnorm(50,0.5,0.65)  ;  y1 <- rnorm(50,mean=0.5,sd=0.65)
x2  <- rnorm(50,1.5,0.65)  ;  y2 <- rnorm(50,mean=1.5,sd=0.65)
sp3 <- matrix(c(x1,x2,y1,y2),ncol=2,byrow=FALSE)
##PLOT DATA
par(mfrow=c(3,1))
plot(sp3,pch=c(rep(1,100),rep(16,100)))

##IDENTIFY CLUSTERS
#eg3 <- ddwtGap(sp3,10,nRep=50)
##PLOT WEIGHTED-GAP AND DD WEIGHTED-GAP
#plot(colMeans(eg3$wGap),pch=15,type='b',xlab="Number of #Clusters",ylab="wtGap",ylim=extendrange(colMeans(eg3$wGap),f=0.2))
#arrows(1:dim(eg3$wGap)[2],colMeans(eg3$wGap)-colMeans(eg3$seGap), 1:dim(eg3$wGap)[2],colMeans(eg3$wGap)+colMeans(eg3$seGap),length=0.05,angle=90,code=3)

#############################################################
##use real data
data(iris)
	
##COMPARE THE RESULTS USING kmeans TO hclust.
irisHC <- ddwtGap(as.matrix(iris[,1:4]),maxClust=10,nRep=50)
irisHC	##consistently finds three well-separated clusters using DDwGap and wGap
#irisKM <- ddwtGap(as.matrix(iris[,1:4]),maxClust=10,nRep=50,method="km")				
#irisKM	##consistently finds only two well-separated clusters using DDwGap; wGap is not consistent. 

##HOW WELL DOES THE hclust METHOD DO?
modelDefn <- cutree(hclust(dist(as.matrix(iris[,1:4]))),3)
tapply(modelDefn,list(modelDefn,iris[,5]),length)
##overlaps on versicolor, but gets I. setosa and I. virginica pretty bang on.
##note that labels in modelDefn (1,2 or 3) are arbitrary.

##TRY THE STRIPPED-DIMENSION DATA	
irisR <- dimReduct(as.matrix(iris[,1:4]))
modelDefn <- cutree(hclust(dist(as.matrix(irisR[[1]]))),3)
tapply(modelDefn,list(modelDefn,iris[,5]),length)
##better

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}{ddwtGap}
