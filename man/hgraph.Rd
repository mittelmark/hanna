\name{hgraph}  
\alias{hgraph}  
\title{ Environment with utility functions to work with graphs.} 
\description{  
  The functions of this envirornment contain a few utility functions to work with graphs 
  represented as adjacency matrix. 
} 
\details{ 
   The following functions are implemented: 
   \itemize{ 
      \item \code{\link{hgraph$average_path_length}} - calculate the average path length for a given adjacency matrix 
      \item \code{\link{hgraph$colors}} - create a color vector based on node degrees  
      \item \code{\link{hgraph$degree}} - calculate the degree centrality for all nodes 
      \item \code{\link{hgraph$eccentricity}} - calculate eccentricity centrality for all nodes 
      \item \code{\link{hgraph$layout}} - calculate a plot layout for an adjacency matrix 
      \item \code{\link{hgraph$new}} - create some example graphs 
      \item \code{\link{hgraph$plot}} - plot an adjacency matrix  
      \item \code{\link{hgraph$shortest_paths}} - calculate the shortest paths between all nodes of an adjacency matrix  
      \item \code{\link{hgraph$triads}} - calculate the number of two and tri edge triads 
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