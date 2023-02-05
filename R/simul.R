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

#' \name{simul_season}
#' \alias{simul_season}
#' \title{ Create matches for everyone against everyone using the given model. }
#' \description{
#'   This function creates pairings for a tournament where in every round.
#'   The actual match will give chances based on a certain amount of tokens in dependence of the given model.
#' }
#' \usage{ simul_season(x,token=rep(5,length(x)),model="null",min.value=4) }
#'
#' \arguments{
#'   \item{x}{ vector of teams }
#'   \item{token}{ vector of token for each team, which might influence the match outcone, depending on the given model, defaults: 5 }
#'   \item{model}{ the model given as string, possible values are 'null', 'chance','gain' or 'last', default: 'null' }
#'   \item{min.value}{ for the model 'last' how low is the minimal value for each team, default: 4}
#' }
#' \details{
#'     This function allows you to create matches for all against all in a season and performs the matches
#'     based on the given model and the current number of tokens for each team.
#' }
#' \value{list with three components: 
#'   \itemize{
#'      \item{M}{matrix of results where 1 is a win, -1 is a loss and 0 is a draw}
#'      \item{token}{vector of current tokens for each team}
#'      \item{model}{the choosen model}
#'   }
#' }
#' \examples{
#' set.seed(123)
#' res=simul_season(LETTERS[1:6],model="null") 
#' res
#' }
#' 

simul_season <- function (x,token=rep(5,length(x)),model='null',min.value=4) {
    pairings=simul_pairings(x)
    nms=x
    names(token)=nms
    x=matrix(0,nrow=length(x),ncol=length(x))
    rownames(x)=colnames(x)=nms
    vec=rownames(x)
    for (i in 1:nrow(pairings)) {
        A=pairings[i,2]
        B=pairings[i,3]
        if (model=="null") {
            smp=c(rep(A,5),rep(B,5)) 
        } else if (model == "chance") {
            smp=c(A,A,B,B,rep(A,token[A]),rep(B,token[B]))
        } else {
            if (token[A]> 0 & token[B] > 0) {
                smp=c(rep(A,token[A]),rep(B,token[B]))
            } else if (token[A]>0) {
                smp=A
            } else if (token[B]>0) {
                smp=B
            } else {
                smp=c()
            }
        }
        if (length(smp) == 0) {
            res=c(0,0)
        } else if (length(smp)==1) {
            if (smp==A) {
                res=c(1,-1)
            } else {
                res=c(-1,1)
            }
        } else if (length(smp)>1) {
            smp=sample(smp,2)
            if (smp[1]!=smp[2]) {
                res=c(0,0)
            } else if (smp[1] == A) {
                res=c(1,-1)
            } else if (smp[1] == B) {
                res=c(-1,1)
            }
        }
        x[A,B]=res[1]
        x[B,A]=res[2]
        token[A]=token[A] + res[1]
        token[B]=token[B] + res[2]
        if (model == "last") {
            if (res[1] == -1) {
                token[A]=min.value
                token[B]=10-min.value
            } else if (res[1] == 1) {
                token[A]=10-min.value
                token[B]=min.value
            } else {
                token[A]=5
                token[B]=5
            }
        }
        if (token[A]<0) {
            token[A]=0
            token[B]=token[B]-1
        }
        if (token[B]<0) {
            token[B]=0
            token[A]=token[A]-1
        }
    }
    return(list(M=x,token=token,model=model))
}

