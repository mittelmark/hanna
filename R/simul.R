# ' \docType{class}
# ' \name{simul}
# ' \alias{simul}
# ' \alias{simul-class}
# ' \title{ Environment obkject with functions do simulate winner-looser
# effects. }
# ' \description{
# ' The functions within the simul environment perform simulations of
# winner-looser
# ' effects for the paper ...
# ' }
# ' \section{Methods}{
# ' \itemize{
# ' \item \code{\link[hanna:simul_compare]{simul$compare}} - compare the
# different models
# ' for a certain number of seasons
# ' \item \code{\link[hanna:simul_graph]{simul$graph}} - create a adjacency
# matrix out of the
# ' results for a match season
# ' \item \code{\link[hanna:simul_pairings]{simul$pairings}} - create round
# pairings for a season
# ' \item \code{\link[hanna:simul_season]{simul$season}} - create matches for
# everyone
# ' against everyone using the given model
# '    }
# ' }
# '
# ' \examples{
# ' set.seed(124)
# ' res=simul$season(LETTERS[1:6],model="null")
# ' res
# ' hgraph$plot(res$M)
# ' } 

simul=new.env()
#' \name{simul$pairings}
#' \alias{simul_pairings}
#' \alias{simul$pairings}
#' \title{ Create matching pairs everyone against every one. }
#' \description{
#'   This function creates pairings for a tournament where in every round
#'   item, teams, etc get new partners for playing.
#' }
#' \usage{ `simul$pairings(x)` }
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
#' simul$pairings(mt)
#' simul$pairings(c("ABA","CDE","EFG"))
#' }
#' 

simul$pairings <- function (x) {
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

#' \name{simul$season}
#' \alias{simul_season}
#' \alias{simul$season}
#' \title{ Create matches for everyone against everyone using the given model. }
#' \description{
#'   This function creates pairings for a tournament where in every round.
#'   The actual match will give chances based on a certain amount of tokens in dependence 
#'   of the given model.
#' }
#' \usage{ `simul$season(x,token=rep(length(x),length(x)),model="null",
#'                            min.value=4,memory=NULL,memory.length=1)` }
#'
#' \arguments{
#'   \item{x}{ vector of teams }
#'   \item{token}{ vector of token for each team, which might influence the match outcone, depending on the given model, defaults: 5 }
#'   \item{model}{ the model given as string, possible values are 'null', 'chance','gain' or 'last', default: 'null' }
#'   \item{min.value}{ for the model 'last' how low is the minimal value for each team, default: 4}
#'   \item{memory}{optional vector of last results for each agent}
#'   \item{memory.length}{how many of last results should be stored, default: 1}
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
#'      \item{memory}{list with last results for each agent}
#'   }
#' }
#' \examples{
#' set.seed(123)
#' res=simul$season(LETTERS[1:6],model="null") 
#' res
#' }
#' 

simul$season <- function (x,token=rep(length(x),length(x)),model='null',min.value=4,memory=NULL,memory.length=1) {
    pairings=simul$pairings(x)
    if (class(memory) == "NULL") {
        memory=lapply(x,function(x) { return(rep(0,memory.length+1)) })
        names(memory)=x
    }
    nms=x
    names(token)=nms
    x=matrix(0,nrow=length(x),ncol=length(x))
    rownames(x)=colnames(x)=nms
    vec=rownames(x)
    for (i in 1:nrow(pairings)) {
        A=pairings[i,2]
        B=pairings[i,3]
        if (model=="null") {
            smp=c(rep(A,length(nms)),rep(B,length(nms)))
        } else if (model == "chance") {
            smp=c(A,A,B,B,rep(A,token[A]),rep(B,token[B]))
        } else if (model == "memory") {
            #tokA=memory.length+sum(memory[[A]])
            #tokB=memory.length+sum(memory[[B]])
            tokA=5+token[[A]]-memory[[A]][length(memory[[A]])]
            tokB=5+token[[B]]-memory[[B]][length(memory[[B]])]
            smp=c(rep(A,tokA),rep(B,tokB)) 
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
        if (model == "memory") {
            #res[1],
            valA=c(token[[A]],memory[[A]])[1:memory.length]
            valB=c(token[[B]],memory[[B]])[1:memory.length]
            memory[[A]]=valA
            memory[[B]]=valB
        }
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
                token[A]=length(nms)
                token[B]=length(nms)
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
    return(list(M=x,token=token,model=model,memory=memory))
}

Simul_season2 <- function (x,memory=NULL,
                           memory.length=0) {
    pairings=simul$pairings(x)
    nms=x
    token=rep(length(nms),length(nms))
    tok=length(nms)
    names(token)=nms
    if (class(memory) != "NULL") {
        token=token+unlist(lapply(memory,sum))
    } else {
        memory=lapply(x,function(x) { return(rep(0,memory.length+1)) })
        names(memory)=nms
    }
    x=matrix(0,nrow=length(x),ncol=length(x))
    rownames(x)=colnames(x)=nms
    vec=rownames(x)
    for (i in 1:nrow(pairings)) {
        A=pairings[i,2]
        B=pairings[i,3]
        tokA=tok+sum(memory[[A]])
        tokB=tok+sum(memory[[B]])
        smp=c(rep(A,tokA),rep(B,tokB)) 
        smp=sample(smp,2)
        if (smp[1]!=smp[2]) {
            res=c(0,0)
        } else if (smp[1] == A) {
            res=c(1,-1)
        } else if (smp[1] == B) {
            res=c(-1,1)
        }
        x[A,B]=res[1]
        x[B,A]=res[2]
        valA=c(res[1],memory[[A]])
        valB=c(res[2],memory[[B]])
        if (memory.length>0) {
            memory[[A]]=valA[1:memory.length]
            memory[[B]]=valB[1:memory.length]
        } 
        token[A]=token[A] + res[1]
        token[B]=token[B] + res[2]
        if (token[A]<0) {
            token[A]=0
            token[B]=token[B]-1
        }
        if (token[B]<0) {
            token[B]=0
            token[A]=token[A]-1
        }
    }
    return(list(M=x,token=token,memory=memory))

}

#' \name{simul$graph}
#' \alias{simul$graph}
#' \alias{simul_graph}
#' \title{ Create a adjacency matrix out of the results for a match season. }
#' \description{
#'   This function creates an adjacency matrix for an undirected or a directed graph out
#'    of the results of season where every team played against every other team.
#'    The edges will be directed from the winning to the loosing team in case the mode is "win" or between
#'    drawing teams in case the mode is "draw".
#' }
#' \usage{ `simul$graph(x,mode="draw")` }
#'
#' \arguments{
#'   \item{x}{ a season matrix with wins encoded as 1, losses as -1 and draws as 0 }
#'   \item{mode}{ either draw or win, default: 'draw' }
#' }
#' \value{ Adjacency matrix } 
#' \examples{
#' set.seed(123)
#' res=simul$season(LETTERS[1:6],model="null") 
#' res$M
#' U = simul$graph(res$M,mode='draw')
#' U
#' D = simul$graph(res$M,mode='win')
#' U
#' }
#' 

simul$graph <- function (x,mode="draw") {
    A=x
    if (mode == "draw") {
        A[A!=0]=2
        A[A==0]=1
        A[A!=1]=0
        diag(A)=0
    } else {
        A[A<0]=0
    }
    return(A)
}


#' \name{simul$compare}
#' \alias{simul$compare}
#' \alias{simul_compare}
#' \title{ Compare the different models for a certain number of seasons. }
#' \description{
#'   This function does a comparison for different models determine
#'   the amount of triads after a certain number of seasons.
#' }
#' \usage{ `simul$compare(n=5,agents=12,seasons=3)` }
#' \arguments{
#'   \item{n}{ how many repeats per model, default: 5}
#'   \item{agents}{how many agents/teams, default: 12} 
#'   \item{seasons}{ow many seasons, default: 3}
#' }
#' 
#' \value{data frame with the results, last column model type}
#' \examples{
#'  set.seed(128)
#'  par(mfrow=c(1,3)) 
#'  res.df=simul$compare(n=5,seasons=3)
#'  for (mod in c("null","chance","gain")) { 
#'    rest=t(scale(t(res.df[res.df$model==mod,1:5])))
#'    boxplot(rest,main=mod,ylim=c(-2,2)) 
#'    lines(1:5,apply(rest,2,median))
#'  }
#' }
simul$compare <- function (n=5,agents=12,seasons=3) {
    nodes=agents
    res.df=data.frame(dd=c(),ds=c(),pa=c(),tr=c(),cy=c())
    plengths=c()
    wlengths=c()
    for (mod in c("null","gain","chance","keystone")) {
        for (i in 1:n) {
            if (mod == "keystone") {
                token=rep(agents,agents)
                token[1]=token[1]*2
                token[2]=token[1]
                names(token)=LETTERS[1:nodes]
                res=simul$season(LETTERS[1:nodes],model=mod,token=token)
            } else {
                res=simul$season(LETTERS[1:nodes],model=mod)
            }
            for (s in 2:seasons) {
                res=simul$season(LETTERS[1:nodes],token=res$token,model=mod)   
            }
            A=res$M
            A[A<0]=0
            A=hgraph$d2u(A)
            pl=hgraph$average_path_length(A,infinite=nrow(A))
            plengths=c(plengths,pl)
            W=Simul_g2w(A)
            wl=hgraph$shortest_paths(W)
            wl[wl==Inf]=2*max(wl[wl!=Inf])
            wl=mean(wl[upper.tri(wl)])
            wlengths=c(wlengths,wl)
            recover()
            res.df=rbind(res.df,t(as.data.frame(unlist(hgraph$triads(simul$graph(res$M,mode="win"))))))
        }   
    }   
    # memory 1, 3, 5
    for (mem in c(1,3,5)) {
        for (i in 1:n) {
            res=simul$season(LETTERS[1:nodes],model="memory",memory.length=mem)
            for (s in 2:seasons) {
                res=simul$season(LETTERS[1:nodes],token=res$token,model="memory",memory=res$memory,memory.length=mem)   
            }
            A=res$M
            A[A<0]=0
            A=hgraph$d2u(A)
            pl=hgraph$average_path_length(A,infinite=nrow(A))
            if (pl==Inf) {
                pl=NA
                #wl[wl==Inf]=2*max(wl[wl!=Inf])
            } 
            plengths=c(plengths,pl)
            W=Simul_g2w(A)
            wl=hgraph$shortest_paths(W)
            if (any(wl==Inf)) {
                wl=NA
                #wl[wl==Inf]=2*max(wl[wl!=Inf])
            } else {
                wl=mean(wl[upper.tri(wl)])
            }
            wlengths=c(wlengths,wl)
            res.df=rbind(res.df,t(as.data.frame(unlist(hgraph$triads(simul$graph(res$M,mode="win"))))))
        }   
    }   
    res.df=cbind(res.df,model=rep(c("null","chance","gain","keystone","memory1","memory3","memory5"),each=n),pls=plengths,wls=wlengths)
    rownames(res.df)=1:nrow(res.df)
    return(res.df)
}

# private functions
Simul_g2w <- function (x) {
    x[x<0]=0
    x[x>0]=1
    u=hgraph$d2u(x)
    degrees = apply(u,1,function(x) { return(length(which(x!= 0))) })
    w=u
    for (i in 1:(nrow(w)-1)) {
        for (j in (i+1):nrow(w)) {
            if (u[i,j]>0) {
                w[i,j]=w[j,i]=1/sqrt((degrees[i]^2+degrees[j]^2))
            }
        }
    }
    return(w)
}
