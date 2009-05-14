\name{splits-package}
\alias{splits-package}
\alias{splits}
\keyword{package}{splits}

\title{SPecies LImits by Threshold Statistics}

\description{
splits contains tools for delimiting species and automated taxonomy at multiple levels of biological organization (eg. DNA barcoding, morphometrics), both top-down (merging phylogenetic and phylogeographic methods) and bottom-up (single samples into more than one groups).  Currently has two wings: one combines phylogenetic and phylogeographic approaches (see \code{\link{gmyc}}), the other identifies the numbers of well-separated groups in a sample (see \code{\link{ddwtGap}})}

\details{
\tabular{ll}{
Package: \tab splits\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-01-16\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
}

\author{Thomas Ezard \email{thomas.ezard [at] imperial [dot] ac [dot] uk}, Tomochika Fujisawa \email{tomochika.fujisawa05[at] imperial [dot] ac [dot] uk} and Tim Barraclough \email{t.barraclough[at] imperial [dot] ac [dot] uk}.

Maintainer: Thomas H.G. Ezard \email{thomas.ezard [at] imperial [dot] ac [dot] uk}.
}

\seealso{\code{\link{ddwtGap}}, \code{\link{gmyc}}}

\examples{
data(test.tr)
test1 <- gmyc(test.tr, method="single", interval=c(0, 10))
summary(test1)

data(iris)
irisR <- dimReduct(iris[,-5])
ddwtGap(as.matrix(irisR),maxClust=10,nRep=50)

}
