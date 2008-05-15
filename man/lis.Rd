\name{lis}
\alias{lis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Longest increasing subsequence for a univariate sample }
\description{
  calculate the size of the longest increasing subsequence for a sample of a continuous random variable.
}
\usage{
lis(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ \code{x} is a  numeric vector of data values }
}
\details{
}
\value{
Integer, the size of the longest increasing subsequence.
}
\references{ A nonparametric idependence test for small sample size }
\author{Jesus Garcia and Veronica Andrea Gonzalez Lopez}
\note{ 
}
\seealso{  }
\examples{
X<-rexp(50) 
Y<-runif(50,0,X)
res<-LIS.pvalue(X,-Y,alternative="two.sided")
res


## The function is currently defined as
function( x ){
	N<-length(x);
	gr<-c(1:N)*0
	gr[1]<-1
	for ( i in 2:N ){
		gr[i] = 1;
		for ( j in 1:(i - 1) ){ 
			if ( x[i] > x[j] ) { gr[i] = max( gr[i], gr[j] + 1) } 
		}
	}
	rr<-max( gr )
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
