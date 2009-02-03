\name{dimReduct}
\alias{dimReduct}
\title{Strip uninformative measurements}
\description{
 Perform PCA, retain axes based on KG or PA, return these values}
\usage{
dimReduct(XX, how = "PA", scale = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{A matrix of measurements.  Each row corresponds to one observation.}
  \item{how}{PA or KG.  defaults to Horn's Parallel Analysis}
  \item{scale}{scale variances?}
}
\details{
Horn's Parallel Analysis using \code{\link[paran]{paran}}.  The Kaiser-Guttman criterion using \code{\link[nFactors]{nScree}}.
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}

\author{Thomas H.G. Ezard \email{thomas.ezard@imperial.ac.uk}, but the hard work by Alexis Dinno (see \code{\link[paran]{paran}}) and Gilles Raiche (see \code{\link[nFactors]{nScree}})}


\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(iris)
dimReduct(as.matrix(iris[,-5],ncol=4))

}
\keyword{utilities}{dimReduct}
