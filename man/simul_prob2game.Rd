\name{simul$prob2game} 
\alias{simul_prob2game} 
\alias{simul$prob2game} 
\title{ Convert probabilities matrices to 0 and 1 matrices } 
\description{ 
  This function takes a given probability matrix and concerts it 
  to a binary matrix where 0 means no game between agents and 1  
  means a game should be performed between the agents. 
} 
\usage{ `simul$prob2game(p)` } 
\arguments{ 
  \item{p}{matrix of probabilities} 
} 
\value{binary matrix, diagonal values are all zero} 
\examples{ 
res=simul$gridAgents() 
P=simul$d2prob(res) 
G=simul$prob2game(P) 
G[1:14,1:14] 
} 
 
