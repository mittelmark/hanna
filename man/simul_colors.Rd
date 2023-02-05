\name{simul_colors} 
\alias{simul_colors} 
\title{ create a color vector based on node degrees } 
\description{ 
  This function creates three colors based on the in- and out-degrees of 
  for the nodes of the given adjacency matrix. nodes with only incoming nodes degrees get the first color, 
  nodes with in and out going edges ge the second color and nodes with 
  only outgoing edges the third color. 
} 
\usage{ simul_colors(x,col=c('grey80','salmon','red')) } 
\arguments{ 
  \item{x}{ adjacency matrix } 
  \item{col}{color vector with three colors, default: c('grey80','salmon','red')} 
} 
\value{ vector of colors with length of node numner } 
\examples{ 
res=simul_season(LETTERS[1:8]) 
D = simul_graph(res$M,mode='win') 
cols=simul_colors(D) 
simul_plot(D,vertex.color=cols) 
} 
\seealso{ \code{\link{testprint}} } 
 
