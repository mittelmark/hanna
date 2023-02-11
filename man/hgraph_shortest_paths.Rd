\name{hgraph$shortest_paths} 
\alias{hgraph$shortest_paths} 
\alias{hgraph_shortest_paths} 
\title{ Calculate the shortest path between all nodes of a graph. } 
\usage{`hgraph$shortest_paths(x,mode="directed",weighted=FALSE,FUN=mean)`} 
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
  \item{weighted}{ should be taken as weighted graph, default: FALSE} 
  \item{FUN}{for directed graphs with multiedges how to compute the entry in undirected graphs} 
} 
\value{ matrix with the pairwise path lengths } 
\examples{ 
set.seed(123) 
A=matrix(rbinom(25,1,p=0.3),ncol=5) 
colnames(A)=rownames(A)=LETTERS[1:5] 
A[lower.tri(A)]=t(A)[lower.tri(A)] 
A 
hgraph$shortest_paths(A,mode="undirected") 
} 
