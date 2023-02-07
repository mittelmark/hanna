#' \name{simul} 
#' \alias{simul} 
#' \title{ Collection of functions which do winner-looser and network structure simulations. } 
#' \description{ 
#'   The function with the simul prefix perform simulations of winner-looser effects for the paper ...
#'   The following functions are implemented:
#'   \itemize{
#'      \item \code{\link{simul_average_path_length}} - calucate the average path length for a given adjacency matrix
#'      \item \code{\link{simul_colors}} - create a color vector based on node degrees 
#'      \item \code{\link{simul_compare}} - compare the different models for a certain number of seasons
#'      \item \code{\link{simul_graph}} - create a adjacency matrix out of the results for a match season
#'      \item \code{\link{simul_layout}} - calculate a plot layout for an adjacency matrix
#'      \item \code{\link{simul_pairings}} - create round pairings for a season
#'      \item \code{\link{simul_plot}} - plot an adjacency matrix 
#'      \item \code{\link{simul_season}} - create matches for everyone against everyone using the given model
#'      \item \code{\link{simul_shortest_paths}} - caluclate the shortest paths between all nodes of an adjacency matrix 
#'      \item \code{\link{simul_triads}} - alculate the number of two and tri edge triads
#'    }
#' }
#'
#' \examples{
#' set.seed(123)
#' res=simul_season(LETTERS[1:6],model="null") 
#' res
#' simul_plot(res$M)
#' }

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
#' \usage{ simul_season(x,token=rep(length(x),length(x)),model="null",min.value=4,memory=NULL,memory.length=1) }
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
#' res=simul_season(LETTERS[1:6],model="null") 
#' res
#' }
#' 

simul_season <- function (x,token=rep(length(x),length(x)),model='null',min.value=4,memory=NULL,memory.length=1) {
    pairings=simul_pairings(x)
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
    pairings=simul_pairings(x)
    nms=x
    token=rep(length(nms),length(nms))
    tok=length(nms)
    names(token)=nms
    if (class(memory) != "NULL") {
        token=token+unlist(lapply(memory,sum))
        print(token)
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
#' \usage{simul_plot(x,layout='sam',
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
#'   \item{weighted}{should the edges be shown with different weights given within the adjacency matrix}
#'   \item{star.center}{for layout 'star' the central node, default: NULL}
#'   \item{\ldots}{arguments delegated to the plot function}
#' }
#' \examples{
#' res=simul_season(LETTERS[1:8])
#' U = simul_graph(res$M,mode='draw')
#' simul_plot(U)
#' D = simul_graph(res$M,mode='win')
#' simul_plot(D)
#' }
#'

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
        vertex.color=rep(vertex.color,nrow(x))
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

#' \name{simul_layout}
#' \alias{simul_layout}
#' \title{ calculate a plot layout for an adjacency matrix }
#' \description{
#'   This function is used to create a layout for a given graph, there are 
#'   a few algorithms available such as MDS based ones like 'mds' or 'sam'
#'   and circular layouts like 'circle' or 'star', furthermore there is 
#'   the possibility to use an interactive mode where the user clicks 
#'   first on a node and then on the space where the node should be moved. 
#'   This interactive mode can be finished by a right click.
#' }
#' \usage{ simul_layout(x,mode='sam', noise=FALSE, star.center=NULL, interactive=FALSE) }
#' \arguments{
#'   \item{x}{an adjacency matrix or an adjacency list}
#'   \item{mode}{either 'mds','sam', 'circle', 'star', default: 'sam'}
#'   \item{noise}{should some scatter been added to the given coordinates, default: FALSE}
#'   \item{star.center}{the node which should be used as center of the star, if not given the first node in the graph will be in the center, default: NULL}
#'   \item{interactive}{should be there an interactive clicking to mode the nodes in the layout, default: FALSE}
#' }
#' \examples{
#' res=simul_season(LETTERS[1:8])
#' U = simul_graph(res$M,mode='draw')
#' lay=simul_layout(U)
#' plot(lay, pch=19,col="salmon",cex=5,xlab="",ylab="",axes=FALSE)
#' text(lay,rownames(U))
#' }
#'

simul_layout <- function (x,mode='sam', noise=FALSE, star.center=NULL,interactive=FALSE) {
    A=x
    if (!identical(A,t(A))) {
        A=D2u(A)
    }
    if (ncol(A)==3 & mode %in% c("sam","mds")) {
        mode="circle"
    }
    if (mode %in% c('mds','sam')) {
        A=Connect(A)
        sp=simul_shortest_paths(A)
        xy=cmdscale(sp)
        rownames(xy)=rownames(A)
        if (mode=='mds') {
            dxy=base::as.matrix(dist(xy))
            diag(dxy)=1
            idx=which(dxy<0.05,arr.ind=TRUE)
            if (nrow(idx)>1) {
                for (i in 1:nrow(idx)) {
                    n=idx[i,1]
                    xy[n,1]=xy[n,1]+rnorm(1,mean=0,sd=0.1)
                    xy[n,2]=xy[n,2]+rnorm(1,mean=0,sd=0.1)
                }
            }
        } else {
            xy=xy+jitter(xy)
            xy=sammon(sp,y=xy,trace=FALSE)$points
        }
    } else if (mode %in% c('circle','star')) {
        x=0
        y=0
        a=0.5
        b=0.5
        rad2deg <- function(rad) {(rad * 180) / (pi)}
        deg2rad <- function(deg) {(deg * pi) / (180)}
        xy=matrix(0,ncol=2,nrow=length(rownames(A)))
        if (mode == "circle") {
            nodes=rownames(A)
            rownames(xy)=nodes
            for (i in 1:length(nodes)) {
                t=deg2rad((360/length(nodes))*(i-1))
                xp = a*cos(t)*0.75 + x;
                yp = b*sin(t)*0.75 + y;
                xy[nodes[i],]=c(xp,yp)
            }
        } else if (mode == 'star') {
            oorder=rownames(A)
            if (class(star.center)[1] != "NULL") {
                norder=c(which(rownames(A)==star.center),which(rownames(A)!=star.center))
                A=A[norder,norder]
            }
            nodes=rownames(A)
            rownames(xy)=nodes
            xy[1,]=c(0.0,0.0)
            for (i in 2:length(nodes)) {
                t=deg2rad((360/(length(nodes)-1))*(i-2))
                xp = a*cos(t)*0.75 + x;
                yp = b*sin(t)*0.75 + y;
                xy[nodes[i],]=c(xp,yp)
            }
            xy=xy[oorder,]
        }
    } else if (mode == 'grid') {
        n=nrow(A)
        xy=matrix(0,ncol=2,nrow=nrow(A))
        rownames(xy)=rownames(A)
        mody=ceiling(sqrt(n))
        x=0
        y=0
        for (r in rownames(A)) {
            if (x %% mody == 0) {
                y=y+1
                x=0
            }
            x=x+1
            xy[r,]=c(x,y)
        }
    } else {
        stop("unknown layout. Use mds, sam, circle, or grid as layout")
    }
    xy=scale(xy)
    if (noise) {
        xy=xy+rnorm(length(xy),mean=0,sd=0.1)
    }
    colnames(xy)=c("x","y")
    doPlot <- function (A,xy) {
        plot(xy,type="n",axes=FALSE,xlab="",ylab="")
        for (i in 1:(ncol(A)-1)) {
            for (j in i:ncol(A)) {
                if (A[i,j]!=0 | A[j,i]!=0) {
                    lines(x=lay[c(i,j),1],y=lay[c(i,j),2])
                }
            }
        }
        points(lay,pch=19,cex=6,col="grey90")
        text(lay,rownames(lay))
        return(lay)
    }
    if (interactive) {
        lay=xy
        lay=doPlot(A,lay)
        print("click two times, first on the point to move, second where to move\nEnd with one or two right clicks!")
        while (TRUE) {
            loc=locator(2)
            if (class(loc) == "NULL" | class(loc$x[2]) == "NULL" | class(loc$x[1]) == "NULL") {
                break
            }
            dlay=rbind(lay,c(loc$x[1],loc$y[1]))
            d=as.matrix(dist(dlay))[nrow(dlay),1:(nrow(dlay)-1)]
            nm=names(which(d==min(d)))[1]
            lay[nm,1]=loc$x[2]
            lay[nm,2]=loc$y[2]
            lay=doPlot(A,lay)
        }
        xy=lay
    }
    return(xy)
}

#' \name{simul_colors}
#' \alias{simul_colors}
#' \title{ create a color vector based on node degrees }
#' \description{
#'   This function creates three colors based on the in- and out-degrees of
#'   for the nodes of the given adjacency matrix. nodes with only incoming nodes degrees get the first color,
#'   nodes with in and out going edges ge the second color and nodes with
#'   only outgoing edges the third color.
#' }
#' \usage{ simul_colors(x,col=c('grey80','indianred1','indianred3')) }
#' \arguments{
#'   \item{x}{ adjacency matrix }
#'   \item{col}{color vector with three colors, default: c('grey80','salmon','red')}
#' }
#' \value{ vector of colors with length of node numner }
#' \examples{
#' set.seed(124)
#' res=simul_season(LETTERS[1:8])
#' D = simul_graph(res$M,mode='win')
#' cols=simul_colors(D)
#' simul_plot(D,vertex.color=cols)
#' }
#' \seealso{ \code{\link{testprint}} }
#'

simul_colors <- function (x,col=c('grey80','indianred1','indianred3')) {
    out=apply(x,1,sum)
    ins=apply(x,2,sum)
    cols=rep(col[1],ncol(x))
    cols[out>0 & ins > 0]=col[2]
    cols[out>0 & ins == 0]=col[3]
    return(cols)
}

#' \name{simul_compare}
#' \alias{simul_compare}
#' \title{ Compare the different models for a certain number of seasons. }
#' \description{
#'   This function does a comparison for different models determine
#'   the amount of triads after a certain number of seasons.
#' }
#' \usage{ simul_compare(n=5,agents=12,seasons=3) }
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
#'  res.df=simul_compare(n=5,seasons=3)
#'  for (mod in c("null","chance","gain")) { 
#'    rest=t(scale(t(res.df[res.df$model==mod,1:5])))
#'    boxplot(rest,main=mod,ylim=c(-2,2)) 
#'    lines(1:5,apply(rest,2,median))
#'  }
#' }
simul_compare <- function (n=5,agents=12,seasons=3) {
    nodes=agents
    res.df=data.frame(dd=c(),ds=c(),pa=c(),tr=c(),cy=c())
    plengths=c()
    for (mod in c("null","chance","gain","keystone")) {
        for (i in 1:n) {
            if (mod == "keystone") {
                token=rep(agents,agents)
                token[1]=token[1]*2
                token[2]=token[1]
                names(token)=LETTERS[1:nodes]
                res=simul_season(LETTERS[1:nodes],model=mod,token=token)
            } else {
                res=simul_season(LETTERS[1:nodes],model=mod)
            }
            for (s in 2:seasons) {
                res=simul_season(LETTERS[1:nodes],token=res$token,model=mod)   
            }
            A=res$M
            A[A<0]=0
            pl=simul_average_path_length(A,mode="directed",infinite=agents)
            plengths=c(plengths,pl)
            res.df=rbind(res.df,t(as.data.frame(unlist(simul_triads(simul_graph(res$M,mode="win"))))))
        }   
    }   
    for (mem in c(1,3,5)) {
        for (i in 1:n) {
            res=simul_season(LETTERS[1:nodes],model="memory",memory.length=mem)
            for (s in 2:seasons) {
                res=simul_season(LETTERS[1:nodes],token=res$token,model="memory",memory=res$memory,memory.length=mem)   
            }
            A=res$M
            A[A<0]=0
            pl=simul_average_path_length(A,mode="directed",infinite=agents)
            plengths=c(plengths,pl)
            res.df=rbind(res.df,t(as.data.frame(unlist(simul_triads(simul_graph(res$M,mode="win"))))))
        }   
    }   
    res.df=cbind(res.df,model=rep(c("null","chance","gain","keystone","memory1","memory3","memory5"),each=n),pls=plengths)
    rownames(res.df)=1:nrow(res.df)
    return(res.df)
}

#' \name{simul_triads}
#' \alias{simul_triads}
#' \title{ Calculate the number of two and tri edge triads. }
#' \usage{ simul_triads(x,percent=FALSE) }
#' \description{
#'   This function calculates the number of two and tri edge triads for
#'    directed graphs. The following triads are possible:
#' \itemize{
#'    \item double-dominant (dd) A->B; A->C
#'    \item double-subordinate (ds) A->B; C->B
#'    \item pass-along (pa) A->B->C
#'    \item transitive (tr) A->B; A->C ; B->C
#'    \item cycle (cy) A->B->C->A
#' }
#' }
#' \arguments{
#'   \item{x}{ adjacency matrix }
#'   \item{percent}{ should the results be return not wihh total numbers but in percent, default: FALSE}
#' }
#' \value{ list object with the following components:
#'  \itemize{
#'    \item{dd}{number of double dominant triads}
#'    \item{ds}{number of double subordinate triads}
#'    \item{pa}{number of pass-along triads}
#'    \item{tr}{number of transitive triads}
#'    \item{cy}{number of cycle triads}
#'  }
#' }
#' \examples{
#' A = matrix(rbinom(49,1,p=0.3),nrow=7)
#' diag(A)=0
#' A[lower.tri(A)]=0
#' rownames(A)=colnames(A)=LETTERS[1:7]
#' simul_plot(A)
#' unlist(simul_triads(A))
#' }

simul_triads <- function (x,percent=FALSE) {
    # count two and tri edge triads
    g=x
    g[g<0]=0
    res=list(dd=0,ds=0,pa=0,tr=0,cy=0)
    if (max(g[upper.tri(g)]+t(g)[upper.tri(g)])>1) {
        stop("Only directed graphs can be used for triad calculations")
    }
    cnames=colnames(g)
    for (i in 1:(ncol(g)-2)) {
        for (j in (i+1):(ncol(g)-1)) {
            for (k in (j+1):(ncol(g))) {
                h=g[c(i,j,k),c(i,j,k)]
                if (sum(h)==2) {
                    if (max(apply(h,1,sum))==2) {
                        res$dd=res$dd+1
                    } else if (max(apply(h,2,sum))==2) {
                        res$ds=res$ds+1
                    } else {
                        res$pa=res$pa+1
                    }
                } else if (sum(h) == 3) {
                    if (max(apply(h,1,sum))==2) {
                        res$tr=res$tr+1
                    } else {
                        res$cy=res$cy+1
                    }
                }
                    
            }
        }
    }
    if (percent) {
        res=as.list((unlist(res)/sum(unlist(res)))*100)
    }
    return(res)
}

#' \name{simul_shortest_paths}
#' \alias{simul_shortest_paths}
#' \title{ Calculate the shortest path between all nodes. }
#' \usage{ simul_shortest_paths(x,mode="directed") }
#' \description{
#'   This function calculates the shortest path between all pair of nodes.
#'    In unconnected graphs it returns Inf, if the connected option is set to TRUE
#'    it returns the average path length for the connected nodes, you can as well
#'    give a value which should be added instead of Inf values, usually this is
#'    the number of nodes.
#' }
#' \arguments{
#'   \item{x}{ adjacency matrix }
#'   \item{mode}{ should be either "directed" or "undirected", default: "directed"}
#' }
#' \value{ matrix with the pairwise path lengths }
#' \examples{
#' set.seed(123)
#' res=simul_season(LETTERS[1:8])
#' M=res$M
#' M[M<0]=0
#' simul_shortest_paths(M)
#' simul_shortest_paths(M,mode="undirected")
#' }

simul_shortest_paths <- function (x,mode="directed") {
    g=x
    A=g
    if (mode == "undirected") {
        A=A+t(A)
        A[A!=0]=1
    }
    S=A
    S[]=Inf
    diag(S)=0
    x=1
    S[A > 0 & A < Inf]=1
    while (TRUE) { 
        flag = FALSE 
        for (m in 1:nrow(S)) {
            ns=which(S[m,] == x)
            for (n in ns) {
                for (o in which(A[n,]==1)) {
                    if (o != m) {
                        flag = TRUE
                        if (S[m,o] > x + 1) {
                            S[m,o]=x+1
                            if (mode == "undirected") {
                                S[o,m]=x+1
                            }
                        }
                    }
                }
            }
        }
        if (!flag) {
            break
        }
        x=x+1
    }
    return(S)
}

Simul_shortest_paths <- function (x,mode="directed",FUNC=max) {
    g=x
    A=g
    if (mode == "undirected") {
        #A=A+t(A)
        #A[A!=0]=1
        for (i in 1:(nrow(A)-1)) {
            for (j in i:nrow(A)) {
                A[i,j]=A[j,i]=FUNC(A[i,j],A[j,i])
            }
        }

    }
    S=A
    S[]=Inf
    diag(S)=0
    x=1
    S[A > 0 & A < Inf]=1
    while (TRUE) { 
        flag = FALSE 
        for (m in 1:nrow(S)) {
            ns=which(S[m,] == x)
            for (n in ns) {
                for (o in which(A[n,]==1)) {
                    if (o != m) {
                        flag = TRUE
                        if (S[m,o] > x + 1) {
                            S[m,o]=x+1
                            if (mode == "undirected") {
                                S[o,m]=x+1
                            }
                        }
                    }
                }
            }
        }
        if (!flag) {
            break
        }
        x=x+1
    }
    return(S)
}

#' \name{simul_average_path_length}
#' \alias{simul_average_path_length}
#' \title{ Calculate the average path length between all nodes. }
#' \usage{ simul_average_path_length(x,mode="directed",unconnected=FALSE,infinite=NULL) }
#' \description{
#'   This function calculates the average shortest path length between all pair of nodes.
#'    In unconnected graphs it returns Inf, if the connected option is set to TRUE
#'    it returns the average path length for the connected nodes, you can as well
#'    give a value which should be added instead of Inf values, usually this is
#'    the number of nodes.
#' }
#' \arguments{
#'   \item{x}{ adjacency matrix }
#'   \item{mode}{ should be either "directed" or "undirected", default: "directed" }
#'   \item{unconnected}{ should only unconnected nodes be used in calculation, default: FALSE}
#'   \item{infinite}{ a value which should replace Inf values, usually it is the number of nodes, default: NULL }
#' }
#' \value{ matrix with the pairwise path lengths }
#' \examples{
#' set.seed(123)
#' res=simul_season(LETTERS[1:8])
#' M=res$M
#' M[M<0]=0
#' simul_shortest_paths(M)
#' simul_average_path_length(M)
#' simul_eccentricity(M)
#' simul_eccentricity(M,unconnected=TRUE)
#' simul_eccentricity(M,mode="undireced",infinite=nrow(M))
#' }
#' 

simul_average_path_length <- function (x,mode="directed", unconnected=FALSE, infinite=NULL) {
     S=simul_shortest_paths(x,mode=mode)
     if (class(infinite) != "NULL") {
         S[S==Inf]=infinite
     }
    s=c(S[upper.tri(S)],S[lower.tri(S)])
    if (unconnected) {
        s=s[s!=Inf]
    }
    return(mean(s))
        
}


#' \name{simul_eccentricity}
#' \alias{simul_eccentricity}
#' \title{ Calculate the eccentricity centrality for all nodes. }
#' \usage{ simul_eccentricity(x,mode="directed",unconnected=FALSE,infinite=NULL) }
#' \description{
#'   This function calculates the inverse of the longest shortest path for each node.
#'    In unconnected graphs it returns Inf, if the connected option is set to TRUE
#'    it returns the value between connected nodes only, you can as well
#'    give a value which should be added instead of Inf values using the infinite argument, usually this is
#'    the number of nodes.
#' }
#' \arguments{
#'   \item{x}{ adjacency matrix }
#'   \item{mode}{ should be either "directed" or "undirected", default: "directed" }
#'   \item{unconnected}{ should only unconnected nodes be used in calculation, default: FALSE}
#'   \item{infinite}{ a value which should replace Inf values, usually it is the number of nodes,default: NULL }
#' }
#' \value{ matrix with the pairwise path lengths }
#' \examples{
#' set.seed(123)
#' res=simul_season(LETTERS[1:8])
#' M=res$M
#' M[M<0]=0
#' simul_shortest_paths(M)
#' simul_average_path_length(M)
#' simul_average_path_length(M)
#' simul_average_path_length(M,unconnected=TRUE)
#' simul_average_path_length(M,mode="undireced",infinite=nrow(M))
#' }
#' 

simul_eccentricity <- function (x,mode="directed", unconnected=FALSE, infinite=NULL) {
    S=simul_shortest_paths(x,mode=mode)
    if (class(infinite) != "NULL") {
        S[S==Inf]=infinite
    }
    vals=apply(S,1,function(x) { s=x; if (unconnected) { s=s[s!=Inf] }; return(1/max(s)) })
    return(vals)
}

#' \name{simul_degree}
#' \alias{simul_degree}
#' \title{ Calculate the degree centrality for all nodes. }
#' \usage{ simul_degree(x,mode="all") }
#' \description{
#'   This function calculates the number of edges connected to a
#'   a node. For directed graph you can distinguish between in and outgoing nodes using the mode argument.
#' }
#' \arguments{
#'   \item{x}{ adjacency matrix }
#'   \item{mode}{ character string, either 'all' for 'in' and 'out' going edges or one of the latter, for undirected graphs the mode is always 'all', default: 'all' }
#' }
#' \value{ numeric vector with the number of connected edges }
#' \examples{
#' set.seed(124)
#' res=simul_season(LETTERS[1:10])
#' M=res$M
#' M[M<0]=0
#' M
#' simul_degree(M)
#' simul_degree(M,mode='in')
#' simul_degree(M,mode='out')
#' }
#' 

simul_degree <- function (x,mode="all") {
    if (mode == "all" | all(x == t(x))) {
        x=D2u(x)
        return(apply(x,1,function (x) { return(length(which(x>0))) }))
    } else if (mode == "in") {
        return(apply(x,2,function(x) { return(length(which(x>0))) }))
    } else if (mode == "out") {
        return(apply(x,2,function(x) { return(length(which(x>0))) }))
    } else {
        stop("Wrong mode, mode must be either 'all', 'in' or 'out'!")
    }
}

# private functions

D2u <- function (g) {
    g[lower.tri(g)]=g[lower.tri(g)]+t(g)[lower.tri(g)]
    g[upper.tri(g)]=g[upper.tri(g)]+t(g)[upper.tri(g)]    
    g[g>0]=1
    g[g<0]=-1
    return(g)
}



Connect = function (g) {
    A=g
    A=as.matrix(A)
    A=A+t(A)
    A[A>0]=1
    P=simul_shortest_paths(A)
    if (!any(P==Inf)) {
        return(A)
    }
    comp=Components(A)
    nodes=c()
    tab=table(comp)
    for (n in names(tab)) {
        c=names(which(comp==n))
        if (tab[[n]] > 2) {
            Am=A[c,c]
            # todo min
            deg=apply(Am,1,sum)
            idx=which(deg>0)
            minval=min(deg[idx])
            idx=which(deg == minval)[1]
            node=c[idx]
        } else {
            node = c[1]
        }
        nodes=c(nodes,node)
    }
    A[nodes,nodes]=1
    diag(A)=0
    return(A)
}

Components <- function (g) {
    A=g
    A=as.matrix(A)
    A=A+t(A)
    A[A>0]=1
    comp=c()
    P=simul_shortest_paths(A)
    nodes=rownames(A)
    x=1
    while (length(nodes) > 0) {
        n=nodes[1]
        idx=which(P[n,] < Inf)
        ncomp=rep(x,length(idx))
        names(ncomp)=rownames(P)[idx]
        comp=c(comp,ncomp)
        nodes=setdiff(nodes,rownames(P)[idx])
        x=x+1
    }
    return(comp[rownames(A)])
}

Simul_g2w <- function (x) {
    x[x<0]=1
    u=D2u(x)
    degrees = apply(u,1,function(x) { return(length(which(x!= 0))) })
    w=u/degrees
    for (i in 1:(nrow(w)-1)) {
        for (j in i:nrow(w)) {
            w[i,j]=w[j,i]=min(w[i,j],w[j,i])
        }
    }
    return(w)
}
