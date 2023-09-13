\name{hgraph$tokenplot} 
\alias{hgraph$tokenplot} 
\alias{hgraph_tokenplot} 
\title{Barplot with default token color codes } 
\usage{`hgraph$tokenplot(x,xlab="token ranges", ylab="proportion", ...)`} 
\description{ 
  This function is used to create a barplot to show the proportion of tokens 
  for nodes in the ranges 0 and 1, 2 to 9, 10 to 20 and 20 or higher with 
  color coding. 
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
 
