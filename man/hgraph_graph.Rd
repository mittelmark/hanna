\name{hgraph$graph} 
\alias{hgraph$graph} 
\alias{hgraph_graph} 
\title{ Create a few example graphs. } 
\usage{`hgraph$graph(type="werner",nodes=10,edges=15)`} 
\description{ 
  This function creates a new graph such as random, kite or werner graphs. 
} 
\arguments{ 
  \item{type}{ eithern a adjacency matrix or a graph type, either 'werner' or 'kite', default: 'werner' } 
  \item{nodes}{for a random graph the number of edges, default: 10} 
  \item{edges}{number of edges in the graph, default: 15} 
} 
\value{ adjacency matrix } 
\examples{ 
A = hgraph$graph() 
A 
} 
 
