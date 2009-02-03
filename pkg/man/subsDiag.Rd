\name{subsDiag}
\alias{subsDiag}

\title{Apply two types of diagnostics to clustered data.}
\description{Calculate diagnostics on the subspace identified by cluster analysis}

\usage{
subsDiag(X, ncl, clustMethod = "hc", nSim = 2000, sigLvl = 0.05, status = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ ~~Describe \code{X} here~~ }
  \item{ncl}{ ~~Describe \code{ncl} here~~ }
  \item{clustMethod}{ ~~Describe \code{clustMethod} here~~ }
  \item{nSim}{ ~~Describe \code{nSim} here~~ }
  \item{sigLvl}{ ~~Describe \code{sigLvl} here~~ }
  \item{status}{ ~~Describe \code{status} here~~ }
}

\details{
Performs model diagnostics
}

\value{
A list containing:
  \item{both}{The index of observations that are BOTH infleuntial and extreme.}
  \item{influence}{The index of infleuntial observations.}
  \item{distance}{The index of extreme observations.}
}
\author{Thomas H.G. Ezard \email{thomas.ezard@imperial.ac.uk}}
\references{
Brooks, S. P. 1994. Diagnostics for Principal Components: Influence Functions as Diagnostic Tools. The Statistician 43:483-494.
Fung, W.-K. 1999. Outlier Diagnostics in Several Multivariate Samples. The Statistician 48:73-84.
}
\seealso{\code{\link{dimReduct}}, \code{\link{ddwtGap}}}
\examples{
##following the example in ddwtGap ....
data(iris)
subsDiag(as.matrix(iris[,1:4]), 3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}{subsDiag}

