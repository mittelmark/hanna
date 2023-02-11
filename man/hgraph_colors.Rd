\name{hgraph$colors} 
\alias{hgraph$colors} 
\alias{hgraph_colors} 
\title{ create a color vector based on node degrees } 
\usage{`hgraph$colors(x,col=c('skyblue','grey80','salmon'))`} 
\description{ 
  This function creates three colors based on the in- and out-degrees of 
  for the nodes of the given adjacency matrix. nodes with only incoming nodes degrees get the first color, 
  nodes with in and out going edges ge the second color and nodes with 
  only outgoing edges the third color. 
} 
\arguments{ 
  \item{x}{ adjacency matrix } 
  \item{col}{color vector with three colors, default: c('grey80','salmon','red')} 
} 
\value{ vector of colors with length of node numner } 
\examples{ 
set.seed(124) 
R=hgraph$new(type="random") 
cols=hgraph$colors(R) 
hgraph$plot(R,vertex.color=cols) 
} 
\seealso{ \code{\link{testprint}} } 
 
