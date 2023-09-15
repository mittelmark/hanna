\name{simul$getProbMatrix} %{{{ 
\alias{simul_getProbMatrix} 
\alias{simul$getProbMatrix} 
\title{Get a probability matrix for games between agents} 
\description{ 
  This function returns a probability matrix for a certain number of agents 
  to express the probabilty that they are matched in a game. 
} 
\usage{ `simul$getProbMatrix(n,sd=1,mode='a')` } 
 
\arguments{ 
  \item{n}{ number of agents } 
  \item{sd}{ data scatter for creating the probabilities, default: 1} 
  \item{mode}{which type of pattern to create, 'a' is based on a simple norm distribution, 'norm' is a two-dimensional normal distribution, 'unif' whill give an uniform distribution, default: 'a'} 
} 
\value{list with the two components: 
  'P' - probability matrix, 
  'layout' - two dimensional layout to plot the points} 
\examples{ 
res=simul$getProbMatrix(6) 
round(res$P,2) 
round(res$layout,2) 
} 
 
