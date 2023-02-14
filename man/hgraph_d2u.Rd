\name{hgraph$d2u} 
\alias{hgraph_d2u} 
\alias{hgraph$d2u} 
\title{ Covert a directed graph into an undirected one. } 
 \usage{`hgraph$d2u(x)`} 
\description{ 
  This function converts an directed graph into an undirected one, the resulting 
  adjacency matrix will be symmetric. 
} 
\arguments{ 
  \item{x}{ adjacency matrix } 
} 
\value{ adjacency matrix } 
\examples{ 
A=hgraph$graph(type="werner") 
A 
U=hgraph$d2u(A) 
par(mfrow=c(1,2)) 
lay=hgraph$layout(A) 
plot(A,layout=lay) 
plot(U,layout=lay) 
} 
 
