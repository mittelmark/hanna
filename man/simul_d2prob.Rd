\name{simul$d2prob} 
\alias{simul_d2prob} 
\alias{simul$d2prob} 
\title{ Convert distances between points to probabilities using Gompertz function } 
\description{ 
  This function calculates for the given coordinates probability values 
  using the Gompertz function. 
} 
\usage{ `simul$d2prob(x, b=50, c=1.5)` } 
 
\arguments{ 
  \item{x}{given x values} 
  \item{b}{displacment value for the Gompertz function, default: 50}  
  \item{c}{growth rate value for the Gompertz function, default: 1.5} 
} 
\value{matrix of probability values based on Gompertz function } 
\examples{ 
res=simul$gridAgents() 
plot(res,pch=19,cex=2,col="blue") 
P=simul$d2prob(res) 
round(P,2)[1:14,1:14] 
} 
 
