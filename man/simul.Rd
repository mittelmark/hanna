\name{simul} 
\alias{simul} 
\alias{simul-class} 
\title{ Environment obkject with functions do simulate winner-looser effects. } 
\description{ 
The functions within the simul environment perform simulations of 
winner-looser effects for the paper  
Hermanussen, M., Dammhahn, M., Scheffler, C. et al. Winner-loser effects improve social network efficiency between competitors with equal resource holding power. Sci Rep 13, 14439 (2023). https://doi.org/10.1038/s41598-023-41225-y 
} 
\section{Methods}{ 
\itemize{ 
\item \code{\link[hanna:simul_checkLayout]{simul$checkLayout}} - check a layout with given probabilities for the number of matches 
\item \code{\link[hanna:simul_checkModel]{simul$checkModel}} - check a model with graphics and triad statistics 
\item \code{\link[hanna:simul_compare]{simul$compare}} - compare the different models for a certain number of seasons 
\item \code{\link[hanna:simul_d2prob]{simul$d2prob}} - convert distances between points to probabilities using Gompertz function 
\item \code{\link[hanna:simul_getProbMatrix]{simul$getProbMatrix}} - get a probability matrix for games between agents 
\item \code{\link[hanna:simul_getNames]{simul$getNames}} - get automatic node names for a certain number of items 
\item \code{\link[hanna:simul_gini]{simul$gini}} - Gini coefficient 
\item \code{\link[hanna:simul_gompertz]{simul$gompertz}} - Gompertz function 
\item \code{\link[hanna:simul_graph]{simul$graph}} - create a adjacency matrix out of the results for a match season 
\item \code{\link[hanna:simul_gridAgents]{simul$gridAgents}} - grid layout of nodes in a network with some noise 
\item \code{\link[hanna:simul_pairings]{simul$pairings}} - create roundpairings for a season 
\item \code{\link[hanna:simul_prob2game]{simul$prob2game}} - convert probabilities matrices to 0 and 1 matrices 
\item \code{\link[hanna:simul_season]{simul$season}} - create matches for everyone against everyone using the given model 
} 
} 
\examples{ 
set.seed(124) 
res=simul$season(LETTERS[1:6],model="null") 
res 
hgraph$plot(res$M) 
}  
