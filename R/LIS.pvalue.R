`LIS.pvalue` <-
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

