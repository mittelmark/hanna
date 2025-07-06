\name{simul$gini} %{{{ 
\alias{simul_gini} 
\alias{simul$gini} 
\title{ Gini coefficient } 
\description{ 
  Returns the Gin coefficient for inequality, where 1 is the highest possible inequality  
  and zero is total equality. 
} 
\usage{ `simul$gini(x)` } 
 
\arguments{ 
  \item{x}{ vector with numerical values } 
  \item{method}{either "glasser-brown" or "lorenz", default = "glasser-brown"} 
} 
\value{computed Gini coefficient } 
\examples{ 
simul$gini(c(1,2,3,4,10)) 
simul$gini(c(1,1,1,1,1)) 
} 
 
