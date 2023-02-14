\name{hgraph}  
\alias{hgraph}  
\alias{hgraph-class} 
\title{ Environment object with utility functions to work with graphs.} 
\description{  
  The functions of this environment contain a few utility functions to work with graphs 
  represented as adjacency matrix. 
} 
\section{Methods}{ 
   The following functions are implemented: 
   \itemize{ 
      \item \code{\link[hanna:hgraph_average_path_length]{hgraph$average_path_length}} - calculate the average path length for a given adjacency matrix 
      \item \code{\link[hanna:hgraph_colors]{hgraph$colors}} - create a color vector based on node degrees  
      \item \code{\link[hanna:hgraph_d2u]{hgraph$d2u}} - convert a directed into an undirected graph 
      \item \code{\link[hanna:hgraph_degree]{hgraph$degree}} - calculate the degree centrality for all nodes 
      \item \code{\link[hanna:hgraph_eccentricity]{hgraph$eccentricity}} - calculate eccentricity centrality for all nodes 
      \item \code{\link[hanna:hgraph_graph]{hgraph$graph}} - create some example graphs 
      \item \code{\link[hanna:hgraph_layout]{hgraph$layout}} - calculate a plot layout for an adjacency matrix 
      \item \code{\link[hanna:hgraph_plot]{hgraph$plot}} - plot an adjacency matrix  
      \item \code{\link[hanna:hgraph_shortest_paths]{hgraph$shortest_paths}} - calculate the shortest paths between all nodes of an adjacency matrix  
      \item \code{\link[hanna:hgraph_triads]{hgraph$triads}} - calculate the number of two and tri edge triads 
   } 
} 
\examples{ 
set.seed(124) 
A=matrix(c(0,0,1,0,0,0, 
           0,0,1,0,0,9, 
           0,0,0,1,0,0, 
           0,0,0,0,1,1, 
           0,0,0,0,0,1, 
           0,0,0,0,0,0),ncol=6,byrow=TRUE) 
colnames(A)=rownames(A)=LETTERS[1:6] 
A 
hgraph$shortest_paths(A) 
hgraph$shortest_paths(A,mode="undirected") 
} 
