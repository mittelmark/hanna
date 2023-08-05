\name{simul$gompertz} 
\alias{simul_gompertz} 
\alias{simul$gompertz} 
\title{ Gompertz function } 
\description{ 
  Returns values for the given vector according to the Gompertz function. 
} 
\usage{ `simul$gompertz(x, a=1, b=0.5, c=0.2)` } 
 
\arguments{ 
  \item{x}{given x values} 
  \item{a}{asymptote, default: 1} 
  \item{b}{displacment on x, default: -0.5}  
  \item{c}{growth rate, default: 0.2} 
} 
\value{computed Gompertz values } 
\examples{ 
simul$gompertz(1:5) 
plot(simul$gompertz(1:20,a=-1,b=50,c=1.5)) 
} 
 
