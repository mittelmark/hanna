\name{hgraph$new} 
\alias{hgraph$new} 
\alias{hgraph_new} 
\title{ Create a few example graphs. } 
\description{ 
  This function creates a new graph su7ch as kite or werner graphs. 
 
  hgraph$new(type="werner",nodes=10,edges=15) 
} 
\arguments{ 
  \item{type}{ eithern a adjacency matrix or a graph type, either 'werner' or 'kite', default: 'werner' } 
  \item{nodes}{for a random graph the number of edges, default: 10} 
  \item{edges}{number of edges in the graph, default: 15} 
} 
\value{ adjacency matrix } 
\examples{ 
A = hgraph$new() 
A 
} 
 
