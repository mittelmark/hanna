\name{hgraph$tokenplot} 
\alias{hgraph$tokenplot} 
\alias{hgraph_tokenplot} 
\title{Barplot with default token color codes } 
\usage{`hgraph$tokenplot(x,xlab="token ranges", ylab="proportion", ...)`} 
\description{ 
  This function is used to create a layout for a given graph. 
  
  There are a few algorithms available such as MDS based ones like 'mds' or 'sam' 
  and circular layouts like 'circle' or 'star', furthermore there is  
  the possibility to use an interactive mode where the user clicks  
  first on a node and then on the space where the node should be moved.  
  This interactive mode can be finished by a right click. 
} 
\arguments{ 
  \item{x}{vector of token values for a set of nodes} 
  \item{xlab}{label for the x-axis, default: "token ranges"} 
  \item{ylab}{label for the y-axis, default: "proportion"} 
  \item{\ldots}{other arguments, delegated to the default barplot function} 
} 
\examples{ 
### could be token values from a simulation 
vec=stats::rlnorm(1000,meanlog=1.5) 
hgraph$tokenplot(vec) 
} 
 
