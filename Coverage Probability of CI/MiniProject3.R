# Sample sizes.
N = c(5, 10,30, 50, 100)
# population proportion
P = c(0.05, 0.1, 0.25, 0.5, 0.9, 0.95)
# Coverage probabilities.
mat<-matrix(list(), nrow=5, ncol=1)
for (k in seq(1, 5, 1))
{
  n = N[k]
  CPS = NULL
  for (j in seq(1, 6, 1))
  {
    p = P[j]
    ci = mean(rnorm(n,95)) + (c(-1,1)*1.96*sqrt((p*(1-p))/n))
    CPS[j]=mean(ci)
  }
  show(n)
  show(CPS)
  mat[[k,1]] <- CPS
}
plot(P,as.numeric(mat[[1]]),type="l",xlab="Population Proportion",ylab="Confidence interval",col="black")
lines(P,as.numeric(mat[[2]]),col="green")
lines(P,as.numeric(mat[[3]]),col="blue")
lines(P,as.numeric(mat[[4]]),col="red")
lines(P,as.numeric(mat[[5]]),col="orange")
legend( x="topright", 
  legend=c("n=5","n=10","n=30","n=50","n=100"),
  col=c("black","green","blue","red","orange"),lwd=1,pch=c(NA,NA),cex = 0.50,ncol = 6)
