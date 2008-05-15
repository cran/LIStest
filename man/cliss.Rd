\name{cliss}
\alias{cliss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Longest increasing subsequence for a bivariate sample }
\description{
calculate the size of the longest increasing subsequence for the samples of continuous random variables.
}
\usage{
cliss(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ \code{x} is a  numeric vector of data values }
  \item{y}{ \code{y} is a  numeric vector of data values }
}
\details{
}
\value{
Integer, the size of the longest increasing subsequence
}
\references{ A nonparametric idependence test for small sample size }
\author{ Jesus Garcia and Veronica Andrea Gonzalez Lopez }
\note{ 
}
\seealso{ }
\examples{
X<-rexp(50) 
Y<-runif(50,0,X)
res<-cliss(X,Y)
res



## The function is currently defined as
function(x,y){
	s<-order(y[order(x)])
	rr<-lis(s)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
