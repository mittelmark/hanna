\name{simul} %{{{ 
\alias{simul} 
\alias{simul-class} 
\title{ Environment obkject with functions do simulate winner-looser effects. } 
\description{ 
The functions within the simul environment perform simulations of 
winner-looser effects for the paper ... 
} 
\section{Methods}{ 
\itemize{ 
\item \code{\link[hanna:simul_compare]{simul$compare}} - compare the different models for a certain number of seasons 
\item \code{\link[hanna:simul_graph]{simul$graph}} - create a adjacency matrix out of the results for a match season 
\item \code{\link[hanna:simul_pairings]{simul$pairings}} - create roundpairings for a season 
\item \code{\link[hanna:simul_season]{simul$season}} - create matches for everyone against everyone using the given model 
} 
} 
\examples{ 
set.seed(124) 
res=simul$season(LETTERS[1:6],model="null") 
res 
hgraph$plot(res$M) 
}  
