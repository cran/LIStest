\name{LIStest-package}
\alias{LIStest-package}
\alias{LIStest}
\docType{package}
\title{
Longest Increasing Subsequence Independence Test
}
\description{
calculate the pvalue for the two samples L.I.S independence test (for continuous random variables).
}
\details{
\tabular{ll}{
Package: \tab LIStest\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-05-15\cr
License:\tab  GPL 2.0 \cr
}

}
\author{
Jesus Garcia and Veronica Andrea Gonzalez Lopez

Maintainer: Who to complain to <jg@ime.unicamp.br>
}
\references{
A nonparametric idependence test for small sample size
}

\keyword{ package }
\seealso{

}
\examples{
X<-rexp(50) 
Y<-runif(50,0,X)
res<-LIS.pvalue(X,-Y,alternative="two.sided")
res

}
