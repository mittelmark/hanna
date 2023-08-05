\name{hgraph$plot} 
\alias{hgraph$plot} 
\alias{hgraph_plot} 
\alias{plot.graph} 
\title{ plot an adjacency matrix representing a graph } 
\usage{`hgraph$plot(x, layout='sam', 
         vertex.size=1, vertex.labels=NULL, vertex.color="grey80", 
         vertex.cex=1, vertex.pch=19, 
         edge.color="grey40", edge.lty=1, edge.text=NULL, edge.cex=1, 
         edge.pch=0, edge.lwd=3,arrows=TRUE,weighted=FALSE, 
         star.center=NULL,...)` 
} 
\description{ 
  This function plots an adjacency matrix representing an undirected or 
  directed graph using different layout mechanisms. 
 
} 
\arguments{ 
  \item{x}{ an adjacency matrix } 
  \item{layout}{either 'mds','sam', 'circle', 'star', 'graphviz' or an two column matrix or data frame with x and y coordinates, if 'graphviz' only teh default output of the graphviz tools can be shown, default: 'sam'} 
  \item{vertex.size}{the size of the vertex circles, default: 1} 
  \item{vertex.labels}{alternative names shown in the vertices, if not given using the rownames of the adjacency matrix, default: NULL} 
  \item{vertex.color}{the color of the vertices, default: 'salmon'} 
  \item{vertex.cex}{the relative font size of the vertice labels, default: 1} 
  \item{vertex.pch}{plotting character for the vertices, default: 19} 
  \item{edge.color}{either a single color or a matrix with color names of the same same shape as the adjacency matrix, default: 'grey40'} 
  \item{edge.lty}{either a single integer 1-4 representing the line type or a matrix with integers in this range of the same same shape as the adjacency matrix, default: 1} 
  \item{edge.text}{optional matrix to give edge labels, default: NULL} 
  \item{edge.cex}{optional character expansion for the edge label and the underlying plotting character, default: 1} 
  \item{edge.pch}{optional character code for the edge.text, default: 0} 
  \item{edge.lwd}{the line width for the edges} 
  \item{arrows}{should edges been show in case graph is directed, default: TRUE} 
  \item{weighted}{should the edges be shown with different weights given within the adjacency matrix} 
  \item{star.center}{for layout 'star' the central node, default: NULL} 
  \item{\ldots}{arguments delegated to the plot function} 
} 
\examples{ 
A=hgraph$graph() 
col=hgraph$colors(A) 
hgraph$plot(A,vertex.color=col) 
} 
 
