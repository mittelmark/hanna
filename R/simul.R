0#' \name{simul} 
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

#' \name{simul_graph}
#' \alias{simul_graph}
#' \title{ Create a adjacency matrix out of the results for a match season. }
#' \description{
#'   This function creates an adjacency matrix for an undirected or a directed graph out
#'    of the results of season where every team played against every other team.
#'    The edges will be directed from the winning to the loosing team in case the mode is "win" or between
#'    drawing teams in case the mode is "draw".
#' }
#' \usage{ simul_graph(x,mode="draw") }
#'
#' \arguments{
#'   \item{x}{ a season matrix with wins encoded as 1, losses as -1 and draws as 0 }
#'   \item{mode}{ either draw or win, default: 'draw'5 }
#' }
#' \value{ Adjacency matrix } 
#' \examples{
#' set.seed(123)
#' res=simul_season(LETTERS[1:6],model="null") 
#' res$M
#' U = simul_graph(res$M,mode='draw')
#' U
#' D = simul_graph(res$M,mode='win')
#' U
#' }
#' 

simul_graph <- function (x,mode="draw") {
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


#' \name{simul_plot}
#' \alias{simul_plot}
#' \title{ plot an adjacency matrix }
#' \description{
#'   This function plots an adjacency matrix representing an undirected or
#'   directed graph using different layout mechanisms.
#' }
#' \usage{simulplot(x,layout='sam',
#'          vertex.size=1,vertex.labels=NULL,vertex.color="grey80",vertex.cex=1,vertex.pch=19,
#'          edge.color="grey40",edge.lty=1,edge.text=NULL,edge.cex=1,edge.pch=0,
#'          edge.lwd=3,weighted=FALSE,
#'          star.center=NULL,...)}
#'
#' \arguments{
#'   \item{x}{ an adjacency matrix }
#'   \item{layout}{either 'mds','sam', 'circle', 'star', 'graphviz' or an two column matrix or data frame with x and y coordinates, if 'graphviz' only teh default output of the graphviz tools can be shown, default: 'sam'}
#'   \item{vertex.size}{the size of the vertex circles, default: 1}
#'   \item{vertex.labels}{alternative names shown in the vertices, if not given using the rownames of the adjacency matrix, default: NULL}
#'   \item{vertex.color}{the color of the vertices, default: 'salmon'}
#'   \item{vertex.cex}{the relative font size of the vertice labels, default: 1}
#'   \item{vertex.pch}{plotting character for the vertices, default: 19}
#'   \item{edge.color}{either a single color or a matrix with color names of the same same shape as the adjacency matrix, default: 'grey40'}
#'   \item{edge.lty}{either a single integer 1-4 representing the line type or a matrix with integers in this range of the same same shape as the adjacency matrix, default: 1}
#'   \item{edge.text}{optional matrix to give edge labels, default: NULL}
#'   \item{edge.cex}{optional character expansion for the edge label and the underlying plotting character, default: 1}
#'   \item{edge.pch}{optional character code for the edge.text, default: 0}
#'   \item{edge.lwd}{the line width for the edges}
#'   \item{star.center}{for layout 'star' the central node, default: NULL}
#'   \item{\ldots}{arguments delegated to the plot function}
#' }

simul_plot = function (x,layout='sam',
                       vertex.size=1,vertex.labels=NULL,vertex.color="grey80",vertex.cex=1,vertex.pch=19,
                        edge.color="grey40",edge.lty=1,edge.text=NULL,edge.cex=1,edge.pch=0,
                        edge.lwd=3,weighted=FALSE,
                        star.center=NULL,...) {
    A=x
    if (is.matrix(layout) | is.data.frame(layout)) {
        if (ncol(layout) != 2) {
            stop("If a layout matrix or data frame is given two columns are required!")
        }
    } else if (layout %in% c("sam","mds","circle","star")) {
        layout=simul_layout(A,mode=layout,star.center=star.center)
    } else {
        stop("Wrong layout. Either a two column matrix or one of 'sam','mds','circle' or 'star' must be given!")
    }
    if (!is.null(vertex.labels)) {
        colnames(A)=rownames(A)=vertex.labels
        rownames(layout)=vertex.labels
    }
    if (length(vertex.color) == 1) {
        vertex.color=rep(vertex.color,nrow(g))
    }
    arrow <- function (x,y,cut=0.6,lwd=2,lty=1,arrow.col="#666666",...) {
        hx <- (1 - cut) * x[1] + cut * x[2]
        hy <- (1 - cut) * y[1] + cut * y[2]
        arrows(hx,hy,x[2],y[2],lwd=lwd,code=0,col=arrow.col,lty=lty,...)
        for (a in c(20,15,10,5)) {
            arrows(x[1],y[1],hx,hy,length=0.06*lwd,angle=a,lwd=lwd,col=arrow.col,lty=lty,...)
        }
    }
    xr=diff(range(layout[,1])/10)
    
    yr=diff(range(layout[,2])/10)
    xlim=c(min(layout[,1])-xr,max(layout[,1])+xr)
    ylim=c(min(layout[,2])-yr,max(layout[,2])+yr)
    g="directed"
    if (identical(A,t(A))) {
        g="undirected"
    }
    plot(layout,xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="",...)
    for (i in 1:nrow(A)) {
        idx=which(A[i,]!= 0)
        for (j in idx) {
            if (g=="undirected" & i > j) {
                next
            }
            wlwd=edge.lwd
            if (weighted) {
                wlwd=edge.lwd+abs(A[i,j])
            }
            if (is.matrix(edge.color)) {
                col=edge.color[i,j]
            } else {
                col=edge.color
            }
            if (length(edge.lty)>1) {
                clty=edge.lty[i,j]
            } else {
                clty=edge.lty
            }
            x=layout[c(i,j),1]
            y=layout[c(i,j),2]
            hx <- 0.5 * x[1] + 0.5 * x[2]
            hy <- 0.5 * y[1] + 0.5 * y[2]
            if (g=="undirected") {
                lines(x,y,lwd=wlwd,col=col,lty=clty)
                if (is.matrix(edge.text)) {
                    jitx=  0 ; #diff(range(axTicks(1)))/16
                    jity= 0 ; diff(range(axTicks(2)))/16
                    if (edge.pch>0) {
                        points(hx,hy,pch=edge.pch,cex=5*edge.cex,col="#cccccc99")
                    }
                    text(hx+jitx,hy+jity,edge.text[i,j],cex=edge.cex,col="#000099")
                }
            } else {
                arrow(layout[c(i,j),1],layout[c(i,j),2],lwd=wlwd,arrow.col=col,lty=clty)
                if (is.matrix(edge.text)) {
                    if (edge.pch>0) {
                        points(hx,hy,pch=edge.pch,cex=5*edge.cex,col="#cccccc99")
                    }
                    text(hx,hy,edge.text[i,j],cex=edge.cex,col="#000099")       
                }

            }
        }
    }
    points(layout,pch=vertex.pch,cex=6*vertex.size+0.4,col="black")
    points(layout,pch=vertex.pch,cex=6*vertex.size,col=vertex.color)
    text(layout,rownames(A),cex=vertex.cex)
}
