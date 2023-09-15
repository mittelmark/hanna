\name{simul$checkLayout} 
\alias{simul_checkLayout} 
\alias{simul$checkLayout} 
\title{ Check and visualize layout with a probability matrix for match statistics} 
\description{ 
  This function is useful for detect the basic properties of a model network to evaluate the matching statistics 
} 
\usage{ `simul$checkLayout(x,game.prob=NULL)` } 
 
\arguments{ 
  \item{x}{layout of a network} 
  \item{game.prob}{probability of games between the agents, if not given all against all is assumed,default: NULL} 
} 
\value{data frame with the triad counts for the different repetitios } 
\examples{ 
grid=simul$gridAgents(4,sd=0.1) 
P=simul$d2prob(grid,mode="euc",d=2.1) 
simul$checkLayout(grid,P) 
} 
 
