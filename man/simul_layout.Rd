\name{simul_layout} 
\alias{simul_layout} 
\title{ calculate a plot layout for an adjacency matrix } 
\description{ 
  This function is used to create a layout for a given graph, there are  
  a few algorithms available such as MDS based ones like 'mds' or 'sam' 
  and circular layouts like 'circle' or 'star', furthermore there is  
  the possibility to use an interactive mode where the user clicks  
  first on a node and then on the space where the node should be moved.  
  This interactive mode can be finished by a right click. 
} 
\usage{ simul_layout(x,mode='sam', noise=FALSE, star.center=NULL, interactive=FALSE) } 
\arguments{ 
  \item{x}{an adjacency matrix or an adjacency list} 
  \item{mode}{either 'mds','sam', 'circle', 'star', default: 'sam'} 
  \item{noise}{should some scatter been added to the given coordinates, default: FALSE} 
  \item{star.center}{the node which should be used as center of the star, if not given the first node in the graph will be in the center, default: NULL} 
  \item{interactive}{should be there an interactive clicking to mode the nodes in the layout, default: FALSE} 
} 
\examples{ 
res=simul_season(LETTERS[1:8]) 
U = simul_graph(res$M,mode='draw') 
lay=simul_layout(U) 
plot(lay, pch=19,col="salmon",cex=5,xlab="",ylab="",axes=FALSE) 
text(lay,rownames(U)) 
} 
 
