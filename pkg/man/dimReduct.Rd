\name{dimReduct}
\alias{dimReduct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Strip uninformative measurements}
\description{
 Perform PCA, retain axes based on KG or PA, return these values}
\usage{
dimReduct(XX, how = "KG", scale = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{A matrix of measurements.  Each row corresponds to one observation.}
  \item{how}{PA or KG}
  \item{scale}{scale variances?}
}
\details{
see above
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}

\author{Thomas H.G. Ezard \email{thomas.ezard@imperial.ac.uk}}


\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(iris)
dimReduct(as.matrix(iris[,-5],ncol=4))

}
\keyword{utilities}{dimReduct}
