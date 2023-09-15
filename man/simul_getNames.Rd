\name{simul$getNames} %{{{ 
\alias{simul_getNames} 
\alias{simul$getNames} 
\title{Get automatic node names for a certain number of items} 
\description{ 
  This function returns names for a certain number of items to be shown 
  for instance in a graph. 
} 
\usage{ `simul$getNames(n)` } 
 
\arguments{ 
  \item{n}{ number of agents } 
} 
\value{character vector with names } 
\examples{ 
simul$getNames(26) 
head(simul$getNames(100)) 
} 
 
