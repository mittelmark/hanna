\name{simul$gridAgents} 
\alias{simul_gridAgents} 
\alias{simul$gridAgents} 
\title{ Grid layout of nodes in a network with some noise } 
\description{ 
  Returns coordinates for a certain nuber of nodes in a regular grid with some added noise to  
  to improve visibility of edges between nodes. 
} 
\usage{ `simul$gridAgents(x)` } 
 
\arguments{ 
  \item{x}{ grid dimension, given value x will create x * x network of coordinates } 
} 
\value{computed Gini coefficient } 
\examples{ 
round(simul$gridAgents(4),2) 
} 
 
