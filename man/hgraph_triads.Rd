\name{hgraph$triads} 
\alias{hgraph$triads} 
\title{ Calculate the number of two and tri edge triads. } 
\usage{`hgraph$triads(x,percent=FALSE)`} 
\description{ 
  This function calculates the number of two and tri edge triads for 
  directed graphs.  
  
  The following triads are possible: 
  \describe{ 
    \item{double-dominant (dd)}{A->B; A->C} 
    \item{double-subordinate (ds)}{A->B; C->B} 
    \item{pass-along (pa)}{A->B->C} 
    \item{transitive (tr)}{A->B; A->C ; B->C} 
    \item{cycle (cy)}{A->B->C->A} 
  } 
} 
\arguments{ 
  \item{x}{ adjacency matrix } 
  \item{percent}{ should the results be return not wihh total numbers but in percent, default: FALSE} 
} 
\value{ list object with the following components: 
   \item{dd}{number of double dominant triads} 
   \item{ds}{number of double subordinate triads} 
   \item{pa}{number of pass-along triads} 
   \item{tr}{number of transitive triads} 
   \item{cy}{number of cycle triads} 
} 
\examples{ 
A = matrix(rbinom(49,1,p=0.3),nrow=7) 
diag(A)=0 
A[lower.tri(A)]=0 
rownames(A)=colnames(A)=LETTERS[1:7] 
hgraph$plot(A) 
unlist(hgraph$triads(A)) 
} 
