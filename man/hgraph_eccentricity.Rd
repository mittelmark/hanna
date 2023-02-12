\name{hgraph$eccentricity} 
\alias{hgraph$eccentricity} 
\alias{hgraph_eccentricity} 
\title{ Calculate the eccentricity centrality for all nodes. } 
\usage{`hgraph$eccentricity(x,mode="directed",unconnected=FALSE,infinite=NULL)`} 
\description{ 
  This function calculates the inverse of the longest shortest path for each node. 
   In unconnected graphs it returns Inf, if the connected option is set to TRUE 
   it returns the value between connected nodes only, you can as well 
   give a value which should be added instead of Inf values using the infinite argument, usually this is 
   the number of nodes. 
} 
\arguments{ 
  \item{x}{ adjacency matrix } 
  \item{mode}{ should be either "directed" or "undirected", default: "directed" } 
  \item{unconnected}{ should only unconnected nodes be used in calculation, default: FALSE} 
  \item{infinite}{ a value which should replace Inf values, usually it is the number of nodes,default: NULL } 
} 
\value{ matrix with the pairwise path lengths } 
\examples{ 
set.seed(123) 
A=hgraph$graph() 
A 
hgraph$eccentricity(A) 
hgraph$eccentricity(A,mode="undireced") 
} 
 
