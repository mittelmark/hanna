\name{simul_compare} 
\alias{simul_compare} 
\title{ Compare the different models for a certain number of seasons. } 
\description{ 
  This function does a comparison for different models determine 
  the amount of triads after a certain number of seasons. 
} 
\usage{ simul_compare(n=5,agents=12,seasons=3) } 
\arguments{ 
  \item{n}{ how many repeats per model, default: 5} 
  \item{agents}{how many agents/teams, default: 12}  
  \item{seasons}{ow many seasons, default: 3} 
} 
 
\value{data frame with the results, last column model type} 
\examples{ 
 set.seed(128) 
 par(mfrow=c(1,3))  
 res.df=simul_compare(n=5,seasons=3) 
 for (mod in c("null","chance","gain")) {  
   rest=t(scale(t(res.df[res.df$model==mod,1:5]))) 
   boxplot(rest,main=mod,ylim=c(-2,2))  
   lines(1:5,apply(rest,2,median)) 
 } 
} 
