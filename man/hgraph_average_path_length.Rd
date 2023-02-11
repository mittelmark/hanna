\name{hgraph$average_path_length} 
\alias{hgraph$average_path_length} 
\alias{hgraph_average_path_length} 
\title{Calculate the average path length between all nodes of a graph. } 
\usage{`hgraph$average_path_length(x,mode="directed",unconnected=FALSE,infinite=NULL)`} 
\description{ 
  This function calculates the average shortest path length between all pair of nodes. 
   In unconnected graphs it returns Inf, if the connected option is set to TRUE 
   it returns the average path length for the connected nodes, you can as well 
   give a value which should be added instead of Inf values, usually this is 
   the number of nodes. 
} 
\arguments{ 
  \item{x}{ adjacency matrix } 
  \item{mode}{ should be either "directed" or "undirected", default: "directed" } 
  \item{unconnected}{ should only unconnected nodes be used in calculation, default: FALSE} 
  \item{infinite}{ a value which should replace Inf values, usually it is the number of nodes, default: NULL } 
} 
\value{ matrix with the pairwise path lengths } 
\examples{ 
set.seed(123) 
x=1 
} 
 
