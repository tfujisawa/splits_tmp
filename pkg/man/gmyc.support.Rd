\name{gmyc.support}
\alias{gmyc.support}
\alias{conf.set}
\alias{akaike.weight}

\title{Calculating AIC-based support values for the GMYC clusters}

\description{
This function calculates support values of species clusters delmited by the GMYC function using information-theoretic multimodel inference. 
}

\usage{
gmyc.support(object, p=0.95)
}

\arguments{
	\item{object}{a gmyc object obtained by gmyc}
	\item{p}{level of confidence. models within p\% confidence set are used to calculate support values}
}

\details{
The function calculate the support values of the GMYC-delimited species by using the multimodel comparison approach described by Burnham & Anderson (2002). The support value of a node is defined as the sum of Akaike weights of candidate delimitation models in which the node is included. Only models included in the p\% confidence set are used for calculation if you specify p. See reference papers for further details. 
}

\value{
\code{gmyc.support} returns a vector containing GMYC support values for tree nodes. Support values are ordered following the node order of input tree.
}

\references{
Fujisawa, T. & Barraclough, TG. in press. Delimiting species using single-locus data and the generalized mixed Yule coalescent (GMYC) approach: A revised method and evaluation on simulated datasets. Systematic Biology.

%Powell JR. 2012. Accounting for uncertainty in species delineation during the analysis of environmental DNA sequence data. Methods in Ecology and Evolution. 3:1-11.

Model selection and multimodel inference: A practical information-theoritic approach. 2nd ed. 2002. Springer Science. New York, USA.
}

\author{ Tomochika Fujisawa \email{t.fujisawa05@gmail.com}}

\examples{
data(test.tr)
test <- gmyc(test.tr, method="single")
nsup <- gmyc.support(test)

#plot support values on tree
plot(test)
nsup[nsup==0] <- NA
nodelabels(round(nsup, 2), cex=0.6, frame="n", adj=1)
}
