\name{comp.delimit}
\alias{comp.delimit}

\title{Comparing the GMYC delimitation with an alternative clustering}

\description{This function calculates similarity between the GMYC and an alternative delimitation for assessment of accuracy. Two measures of similarity are available.}

\usage{
comp.delimit(obj, alt, method="EM")
}

\arguments{
	\item{obj}{a gmyc object obtained by \code{gmyc}}
	\item{alt}{a data frame of associations between samples and species representing a delimitation. The first column must be species identifiers and the second column must be samples}
	\item{method}{a character string specifying the method to measure similarity of delimitations; must be "EM" or "NMI" }
}

\details{
The function measures partitioning similarity between the GMYC delimitation and an user specified grouping (eg. taxonomic species). The available measures of partitioning distance is following: 
\itemize{ 
	\item "EM": This counts the number of species which have exact match (EM) between the GMYC and alternative delimitation used in Fujisawa & Barraclough (2013) paper.
	\item "NMI": This calculates the normalized mutual information (NMI) between the GMYC and an alternative delimitation. The method is described in Vinh et al.(2010). 
}
} 

\references{
Fujisawa, T. & Barraclough, TG. 2013. Delimiting species using single-locus data and the generalized mixed Yule coalescent (GMYC) approach: A revised method and evaluation on simulated datasets. Systematic Biology. 62(5): 707-724.

Vinh, NX., Epps, J. & Bailey, J. 2010. Information theoretic measures for clustering comparison: variants, properties, normalization and correction for chance. Journal of Machne Learning Research. 11: 2837-2854.

}

\author{Tomochika Fujisawa \email{t.fujisawa05@gmail.com} and Jiajie Zhang \email{Jiajie.Zhang@h-its.org}}

\examples{
data(test.tr)
test <- gmyc(test.tr, method="single")

#true species names
truesp <- sapply(strsplit(test.tr$tip.label, "\\\."), head, 1) 
truesp <- data.frame(truesp, test.tr$tip.label)

#compare 
comp.delimit(test, truesp, method="EM") #count exact match
comp.delimit(test, truesp, method="NMI") #calculate NMI
}


