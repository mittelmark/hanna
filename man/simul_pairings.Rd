\name{simul_pairings} 
\alias{simul_pairings} 
\title{ Create matching pairs everyone against every one. } 
\description{ 
  This function creates pairings for a tournament where in every round 
  item, teams, etc get new partners for playing. 
} 
\usage{ simul_pairings(x) } 
 
\arguments{ 
  \item{x}{ symmetric matrix with row and colum names being the same, or a vector of names } 
} 
\details{ 
    This function allows you to create pairings between different teams or players for a tournament. 
} 
\value{data frame with the pairings for each round} 
\examples{ 
mt=matrix(0,nrow=4,ncol=4) 
rownames(mt)=colnames(mt)=LETTERS[1:4] 
simul_pairings(mt) 
simul_pairings(c("ABA","CDE","EFG")) 
} 
 
