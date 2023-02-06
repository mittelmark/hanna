\name{simul_shortest_paths} 
\alias{simul_shortest_paths} 
\title{ Calculate the shortest path between all nodes. } 
\usage{ simul_shortest_paths(x,mode="directed") } 
\description{ 
  This function calculates the shortest path between all pair of nodes. 
   In unconnected graphs it returns Inf, if the connected option is set to TRUE 
   it returns the average path length for the connected nodes, you can as well 
   give a value which should be added instead of Inf values, usually this is 
   the number of nodes. 
} 
\arguments{ 
  \item{x}{ adjacency matrix } 
  \item{mode}{ should be either "directed" or "undirected", default: "directed"} 
} 
\value{ matrix with the pairwise path lengths } 
\examples{ 
set.seed(123) 
res=simul_season(LETTERS[1:8]) 
M=res$M 
M[M<0]=0 
simul_shortest_paths(M) 
simul_shortest_paths(M,mode="undirected") 
} 
