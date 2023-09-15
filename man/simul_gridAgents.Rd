\name{simul$gridAgents} 
\alias{simul_gridAgents} 
\alias{simul$gridAgents} 
\title{ Grid layout of nodes in a network with some noise } 
\description{ 
  Returns coordinates for a certain nuber of nodes in a regular grid with some added noise to  
  to improve visibility of edges between nodes. 
} 
\usage{ `simul$gridAgents(x,sd=0.1)` } 
 
\arguments{ 
  \item{x}{ grid dimension, given value x will create x * x network of coordinates } 
  \item{sd}{ standard deviation for the scatter ofthe points, default: 0.1} 
} 
\value{matrix with coordinate columns x and y} 
\examples{ 
round(simul$gridAgents(4),2) 
} 
 
