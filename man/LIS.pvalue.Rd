\name{LIS.pvalue}
\alias{LIS.pvalue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ P value for the longest increasing subsequence (L.I.S) independence test}
\description{
This function calculate the pvalue for the two samples L.I.S independence test for continuous random variables.
}
\usage{
LIS.pvalue(x, y, alternative = c("two.sided", "less", "greater"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ \code{x} a numeric vector of data values (size less than 70)}
  \item{y}{ \code{y} a numeric vector of data values (same size as \code{x})}
  \item{alternative}{ \code{alternative} description of the alternative hypothesis  }
}
\details{
}
\value{
a number between 0 and 1 corresponding to the calculated p-value  
}
\references{  A nonparametric idependence test for small sample size }
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
function (x, y, alternative = c("two.sided", "less", "greater")) 
{
	data(MODE.LN,CUM.LN)
	n <- length(x)
	m <- length(y)
	if (n != m) stop("x and y should have the same size")
    	if (n < 1) stop("not enough data")
	if (n >= 69) stop("sample size must be lesser than 70")
	if (length(unique(c(x,y))) < n)  stop("cannot compute p-values with ties")
	alternative <- match.arg(alternative)
	if(alternative=="greater"){
		ln<-cliss(x,y)
		pvalue<-1-CUM.LN[n,ln]
	}
	if(alternative=="less"){
		ln<-cliss(x,-y)
		pvalue<-1-CUM.LN[n,ln]
	}
	if(alternative=="two.sided"){
		ln<-cliss(x,y)
		if(ln<MODE.LN[n]){pvalue<-CUM.LN[n,ln]/CUM.LN[n,MODE.LN[n]]}
		if(ln>MODE.LN[n]){pvalue<-(1-CUM.LN[n,ln])/(1-CUM.LN[n,MODE.LN[n]])}
		if(ln==MODE.LN[n]){pvalue<-1}
	}	
	res<-pvalue
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
