\name{simul_degree} 
\alias{simul_degree} 
\title{ Calculate the degree centrality for all nodes. } 
\usage{ simul_degree(x,mode="all") } 
\description{ 
  This function calculates the number of edges connected to a 
  a node. For directed graph you can distinguish between in and outgoing nodes using the mode argument. 
} 
\arguments{ 
  \item{x}{ adjacency matrix } 
  \item{mode}{ character string, either 'all' for 'in' and 'out' going edges or one of the latter, for undirected graphs the mode is always 'all', default: 'all' } 
} 
\value{ numeric vector with the number of connected edges } 
\examples{ 
set.seed(124) 
res=simul_season(LETTERS[1:10]) 
M=res$M 
M[M<0]=0 
M 
simul_degree(M) 
simul_degree(M,mode='in') 
simul_degree(M,mode='out') 
} 
 
