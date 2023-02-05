#' \name{simul} 
#' \alias{simul} 
#' \title{ Collection of functions which do winner-looser and network structure simulations. } 
#' \description{ 
#'   The function with the simul prefix perform simulations of winner-looser effects for the paper ...
#'   The following functions are implemented:
#'   \itemize{
#'      \item \code{\link{simul_pairings}} - create round pairings for a season
#'    }
#' }
#'

""
#' \name{simul_pairings}
#' \alias{simul_pairings}
#' \title{ Create matching pairs everyone against every one. }
#' \description{
#'   This function creates pairings for a tournament where in every round
#'   item, teams, etc get new partners for playing.
#' }
#' \usage{ simul_pairings(x) }
#'
#' \arguments{
#'   \item{x}{ symmetric matrix with row and colum names being the same, or a vector of names }
#' }
#' \details{
#'     This function allows you to create pairings between different teams or players for a tournament.
#' }
#' \value{data frame with the pairings for each round}
#' \examples{
#' mt=matrix(0,nrow=4,ncol=4)
#' rownames(mt)=colnames(mt)=LETTERS[1:4]
#' simul_pairings(mt)
#' simul_pairings(c("ABA","CDE","EFG"))
#' }
#' 

simul_pairings <- function (x) {
    if (is.matrix(x)  | is.data.frame(x)) {
        M=x
    } else {
        M=matrix(0,nrow=length(x),ncol=length(x))  
        rownames(M)=colnames(M)=x
    }
    vec=rownames(M)
    df=data.frame(round=c(),A=c(),B=c())
    for (i in 1:(nrow(M)-1)) {
        for (j in 1:(nrow(M)/2)) { 
            A=vec[j]
            B=vec[(nrow(M)+1)-j]
            df=rbind(df,data.frame(round=i,A=A,B=B))
        }
        cvec=vec; 
        cvec[2]=vec[length(vec)]; 
        cvec[3:length(vec)]=vec[2:(length(vec)-1)] ; 
        vec=cvec ; 
    }
    return(df)
}
