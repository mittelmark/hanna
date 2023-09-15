\name{simul$checkModel} 
\alias{simul_checkModel} 
\alias{simul$checkModel} 
\title{ Evaluate the performance of simulation models with summaries and graphically } 
\description{ 
  This function evaluates given model parameters and visualizes them in some plots. 
} 
\usage{ `simul$checkModel(x,itersteps=c(1,5,10,30), n=5,layout=NULL,...)` } 
 
\arguments{ 
  \item{x}{given agents names} 
  \item{itersteps}{vector of numbers to define which iteration steps to monitor, the highest value is the maximum number of iterations, default: c(1,5,10,30)}  
  \item{n}{number of repetitions for model evaluation, default: 5} 
  \item{layout}{two dimensional matrix for the x and y coordinates to display, default: NULL}  
  \item{\ldots}{Arguments delegated to the `simul$season` function} 
} 
\value{data frame with the triad counts for the different repetitios } 
\examples{ 
grid=simul$gridAgents(4,sd=0.1) 
P=simul$d2prob(grid,mode="euc",d=2.1) 
res=simul$checkModel(grid,season="random",model="gain",game.prob=P) 
head(res) 
aggregate(res,by=list(res$season),mean) 
} 
 
