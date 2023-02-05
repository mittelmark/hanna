\name{simul_triads} 
\alias{simul_triads} 
\title{ Calculate the number of two and tri edge triads. } 
\usage{ simul_triads(x) } 
\description{ 
  This function calculates the number of two and tri edge triads for 
   directed graphs. The following triads are possible: 
\itemize{ 
   \item double-dominant (dd) A->B; A->C 
   \item double-subordinate (ds) A->B; C->B 
   \item pass-along (pa) A->B->C 
   \item transitive (tr) A->B; A->C ; B->C 
   \item cycle (cy) A->B->C->A 
} 
} 
\arguments{ 
  \item{x}{ adjacency matrix } 
} 
\value{ list object with the following components: 
 \itemize{ 
   \item{dd}{number of double dominant triads} 
   \item{ds}{number of double subordinate triads} 
   \item{pa}{number of pass-along triads} 
   \item{tr}{number of transitive triads} 
   \item{cy}{number of cycle triads} 
 } 
} 
\examples{ 
A = matrix(rbinom(49,1,p=0.3),nrow=7) 
diag(A)=0 
A[lower.tri(A)]=0 
rownames(A)=colnames(A)=LETTERS[1:7] 
simul_plot(A) 
unlist(simul_triads(A)) 
} 
