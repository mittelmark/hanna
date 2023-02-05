\name{simul_graph} 
\alias{simul_graph} 
\title{ Create a adjacency matrix out of the results for a match season. } 
\description{ 
  This function creates an adjacency matrix for an undirected or a directed graph out 
   of the results of season where every team played against every other team. 
   The edges will be directed from the winning to the loosing team in case the mode is "win" or between 
   drawing teams in case the mode is "draw". 
} 
\usage{ simul_graph(x,mode="draw") } 
 
\arguments{ 
  \item{x}{ a season matrix with wins encoded as 1, losses as -1 and draws as 0 } 
  \item{mode}{ either draw or win, default: 'draw'5 } 
} 
\value{ Adjacency matrix }  
\examples{ 
set.seed(123) 
res=simul_season(LETTERS[1:6],model="null")  
res$M 
U = simul_graph(res$M,mode='draw') 
U 
D = simul_graph(res$M,mode='win') 
U 
} 
 
