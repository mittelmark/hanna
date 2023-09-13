#' \docType{class}
#' \name{hgraph} % {{{1 
#' \alias{hgraph} 
#' \alias{hgraph-class}
#' \title{ Environment object with utility functions to work with graphs.}
#' \description{ 
#'   The functions of this environment contain a few utility functions to work with graphs
#'   represented as adjacency matrix.
#' }
#' \section{Methods}{
#'    The following functions are implemented:
#'    \itemize{
#'       \item \code{\link[hanna:hgraph_average_path_length]{hgraph$average_path_length}} - calculate the average path length for a given adjacency matrix
#'       \item \code{\link[hanna:hgraph_colors]{hgraph$colors}} - create a color vector based on node degrees 
#'       \item \code{\link[hanna:hgraph_d2u]{hgraph$d2u}} - convert a directed into an undirected graph
#'       \item \code{\link[hanna:hgraph_degree]{hgraph$degree}} - calculate the degree centrality for all nodes
#'       \item \code{\link[hanna:hgraph_eccentricity]{hgraph$eccentricity}} - calculate eccentricity centrality for all nodes
#'       \item \code{\link[hanna:hgraph_graph]{hgraph$graph}} - create some example graphs
#'       \item \code{\link[hanna:hgraph_layout]{hgraph$layout}} - calculate a plot layout for an adjacency matrix
#'       \item \code{\link[hanna:hgraph_plot]{hgraph$plot}} - plot an adjacency matrix 
#'       \item \code{\link[hanna:hgraph_shortest_paths]{hgraph$shortest_paths}} - calculate the shortest paths between all nodes of an adjacency matrix 
#'       \item \code{\link[hanna:hgraph_tokenplot]{hgraph$tokenplot}} - colored barplot to show token distribution
#'       \item \code{\link[hanna:hgraph_triads]{hgraph$triads}} - calculate the number of two and tri edge triads
#'    }
#' }
#' \examples{
#' set.seed(124)
#' A=matrix(c(0,0,1,0,0,0,
#'            0,0,1,0,0,9,
#'            0,0,0,1,0,0,
#'            0,0,0,0,1,1,
#'            0,0,0,0,0,1,
#'            0,0,0,0,0,0),ncol=6,byrow=TRUE)
#' colnames(A)=rownames(A)=LETTERS[1:6]
#' A
#' hgraph$shortest_paths(A)
#' hgraph$shortest_paths(A,mode="undirected")
#' }


hgraph=new.env()

# }}}

#' \name{hgraph$eccentricity} %{{{
#' \alias{hgraph$eccentricity}
#' \alias{hgraph_eccentricity}
#' \title{ Calculate the eccentricity centrality for all nodes. }
#' \usage{`hgraph$eccentricity(x,mode="directed",unconnected=FALSE,infinite=NULL)`}
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
#' A=hgraph$graph()
#' A
#' hgraph$eccentricity(A)
#' hgraph$eccentricity(A,mode="undireced")
#' }
#' 
#' %}}}

hgraph$eccentricity <- function (x,mode="directed", unconnected=FALSE, infinite=NULL) {
    self=hgraph
    S=self$shortest_paths(x,mode=mode)
    if (class(infinite) != "NULL") {
        S[S==Inf]=infinite
    }
    vals=apply(S,1,function(x) { s=x; if (unconnected) { s=s[s!=Inf] }; return(1/max(s)) })
    return(vals)
}

#' \name{hgraph$graph}
#' \alias{hgraph$graph}
#' \alias{hgraph_graph}
#' \title{ Create a few example graphs. }
#' \usage{`hgraph$graph(type="werner",nodes=10,edges=15)`}
#' \description{
#'   This function creates a new graph such as random, kite or werner graphs.
#' }
#' \arguments{
#'   \item{type}{ eithern a adjacency matrix or a graph type, either 'werner' or 'kite', default: 'werner' }
#'   \item{nodes}{for a random graph the number of edges, default: 10}
#'   \item{edges}{number of edges in the graph, default: 15}
#' }
#' \value{ adjacency matrix }
#' \examples{
#' A = hgraph$graph()
#' A
#' }
#' 

hgraph$graph <- function (type="werner",nodes=10,edges=15) {
    self=hgraph
    if (is.matrix(type)) {
        if (is.null(rownames(type)[1])) {
            rownames(type)=colnames(type)=LETTERS[1:ncol(type)]
            A= type
        }
    } else if (type == "werner") {
        A=matrix(c(0,0,1,0,0,0,
                   0,0,1,0,0,0,
                   0,0,0,1,0,0,
                   0,0,0,0,1,1,
                   0,0,0,0,0,1,
                   0,0,0,0,0,0),ncol=6,byrow=TRUE)
        colnames(A)=rownames(A)=LETTERS[1:6]
    } else if (type == "kite") {
        A=matrix(0,ncol=10,nrow=10)
        A[0+1,c(1,2,3,5)+1] = 1
        A[1+1,c(0,3,4,6)+1] = 1
        A[2+1,c(0,3,5)+1]   = 1
        A[3+1,c(0:6)+1]     = 1
        A[4+1,c(1,3,6)+1]   = 1
        A[5+1,c(0,2,3,6,7)+1] = 1
        A[6+1,c(1,3,4,5,7)+1] = 1
        A[7+1,c(5,6,8)+1]     = 1
        A[8+1,c(7,9)+1]       = 1
        A=A+t(A)
        A[A>0]=1
        colnames(A)=rownames(A)=LETTERS[1:10]
    } else if (type=="random") {
        if (nodes>26) {
            stop("Only graphs with up to 26 nodes are supported!")
        }
        A=matrix(0,nrow=nodes,ncol=nodes) 
        idx=sample(1:(((nodes*nodes)/2)-nodes),edges)
        A[upper.tri(A)][idx]=1
        colnames(A)=rownames(A)=LETTERS[1:nodes]
    }  else {
        stop("Only kite, random and werner graphs are currently supported!")
    }
    class(A)=c("graph","matrix")
    return(A)
}

#' \name{hgraph$shortest_paths}
#' \alias{hgraph$shortest_paths}
#' \alias{hgraph_shortest_paths}
#' \title{ Calculate the shortest path between all nodes of a graph. }
#' \usage{`hgraph$shortest_paths(x,mode="directed",weighted=FALSE,FUN=mean)`}
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
#'   \item{weighted}{ should be taken as weighted graph, default: FALSE}
#'   \item{FUN}{for directed graphs with multiedges how to compute the entry in undirected graphs}
#' }
#' \value{ matrix with the pairwise path lengths }
#' \examples{
#' set.seed(123)
#' A=matrix(rbinom(25,1,p=0.3),ncol=5)
#' colnames(A)=rownames(A)=LETTERS[1:5]
#' A[lower.tri(A)]=t(A)[lower.tri(A)]
#' A
#' hgraph$shortest_paths(A,mode="undirected")
#' }

hgraph$shortest_paths <- function (x,mode="directed",weighted=FALSE,FUN=mean) {
    self=hgraph
    A=x
    A[A==-1]=0
    if (mode == "undirected" & !weighted) {
        A=self$d2u(A)
    } else if (mode == "undirected") {
        for (i in 1:(nrow(A)-1)) {
            for (j in i:nrow(A)) {
                A[i,j]=A[j,i]=FUN(A[i,j],A[j,i])
            }
        }
    }
    S=ShortestPath_FloydWarshall(A) 
    return(S)
}

ShortestPath_FloydWarshall <- function (A) {
    U=A
    A[A==0]=Inf
    diag(A)=0
    for (k in 1:nrow(A)) {
        for (i in 1:nrow(A)) {
            for (j in 1:nrow(A)) {
                if (A[i,k] + A[k,j] < A[i,j]) {
                    A[i,j] = A[i,k] + A[k,j]
                }
            }
        }
    }
    return(A)
}

#' \name{hgraph$average_path_length}
#' \alias{hgraph$average_path_length}
#' \alias{hgraph_average_path_length}
#' \title{Calculate the average path length between all nodes of a graph. }
#' \usage{`hgraph$average_path_length(x,mode="directed",unconnected=FALSE,infinite=NULL)`}
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
#' x=1
#' }
#' 

hgraph$average_path_length <- function (x,mode="directed", unconnected=FALSE, infinite=NULL) {
    self=hgraph
    S=self$shortest_paths(x,mode=mode)
    if (class(infinite) != "NULL") {
        S[S==Inf]=infinite
    }
    s=c(S[upper.tri(S)],S[lower.tri(S)])
    if (unconnected) {
        s=s[s!=Inf]
    }
    return(mean(s))
}

#' \name{hgraph$colors}
#' \alias{hgraph$colors}
#' \alias{hgraph_colors}
#' \title{ create a color vector based on node degrees }
#' \usage{`hgraph$colors(x,col=c('skyblue','grey80','salmon'))`}
#' \description{
#'   This function creates three colors based on the in- and out-degrees of
#'   for the nodes of the given adjacency matrix. nodes with only incoming nodes degrees get the first color,
#'   nodes with in and out going edges ge the second color and nodes with
#'   only outgoing edges the third color.
#' }
#' \arguments{
#'   \item{x}{ adjacency matrix }
#'   \item{col}{color vector with three colors, default: c('grey80','salmon','red')}
#' }
#' \value{ vector of colors with length of node numner }
#' \examples{
#' set.seed(124)
#' R=hgraph$graph(type="random")
#' cols=hgraph$colors(R)
#' hgraph$plot(R,vertex.color=cols)
#' }
#'

hgraph$colors <- function (x,col=c('skyblue','grey80','salmon')) {
    self=hgraph
    out=apply(x,1,sum)
    ins=apply(x,2,sum)
    cols=rep(col[1],ncol(x))
    cols[out>0 & ins > 0]=col[2]
    cols[out>0 & ins == 0]=col[3]
    return(cols)
}

#' \name{hgraph$d2u}
#' \alias{hgraph_d2u}
#' \alias{hgraph$d2u}
#' \title{ Covert a directed graph into an undirected one. }
#'  \usage{`hgraph$d2u(x)`}
#' \description{
#'   This function converts an directed graph into an undirected one, the resulting
#'   adjacency matrix will be symmetric.
#' }
#' \arguments{
#'   \item{x}{ adjacency matrix }
#' }
#' \value{ adjacency matrix }
#' \examples{
#' A=hgraph$graph(type="werner")
#' A
#' U=hgraph$d2u(A)
#' par(mfrow=c(1,2))
#' lay=hgraph$layout(A)
#' plot(A,layout=lay)
#' plot(U,layout=lay)
#' }
#' 

hgraph$d2u <- function (g) {
    g[lower.tri(g)]=g[lower.tri(g)]+t(g)[lower.tri(g)]
    g[upper.tri(g)]=g[upper.tri(g)]+t(g)[upper.tri(g)]    
    g[g>0]=1
    g[g<0]=-1
    return(g)
}



#' \name{hgraph$degree}
#' \alias{hgraph_degree}
#' \alias{hgraph$degree}
#' \title{ Calculate the degree centrality for all nodes. }
#'  \usage{`hgraph$degree(x,mode="all")`}
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
#' A=hgraph$graph(type="werner")
#' hgraph$degree(A)
#' hgraph$degree(A,mode='in')
#' hgraph$degree(A,mode='out')
#' }
#' 

hgraph$degree <- function (x,mode="all") {
    self=hgraph
    if (mode == "all" | all(x == t(x))) {
        x=self$d2u(x)
        return(apply(x,1,function (x) { return(length(which(x>0))) }))
    } else if (mode == "in") {
        return(apply(x,2,function(x) { return(length(which(x>0))) }))
    } else if (mode == "out") {
        return(apply(x,2,function(x) { return(length(which(x>0))) }))
    } else {
        stop("Wrong mode, mode must be either 'all', 'in' or 'out'!")
    }
}

#' \name{hgraph$triads}
#' \alias{hgraph$triads}
#' \title{ Calculate the number of two and tri edge triads. }
#' \usage{`hgraph$triads(x,percent=FALSE)`}
#' \description{
#'   This function calculates the number of two and tri edge triads for
#'   directed graphs. 
#'  
#'   The following triads are possible:
#'   \itemize{
#'     \item double-dominant (dd) A->B; A->C
#'     \item double-subordinate (ds) A->B; C->B
#'     \item pass-along (pa) A->B->C
#'     \item transitive (tr) A->B; A->C ; B->C
#'     \item cycle (cy) A->B->C->A
#'   }
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
#' hgraph$plot(A)
#' unlist(hgraph$triads(A))
#' }

hgraph$triads <- function (x,percent=FALSE) {
    self=hgraph
    # count two and tri edge triads
    g=x
    g[g<0]=0
    res=list(dd=0,ds=0,pa=0,tr=0,cy=0)
    if (max(g[upper.tri(g)]+t(g)[upper.tri(g)])>1) {
        stop("Only directed graphs can be used for triad calculations")
    }
    cnames=colnames(g)
    # that does not do much improvement
    ng=ncol(g)
    ng2=ng-2
    ng1=ng-1
    for (i in 1:ng2) {
        for (j in (i+1):ng1) {
            for (k in (j+1):ng) {
                h=g[c(i,j,k),c(i,j,k)]
                sh=sum(h)
                if (sh==2) {
                    if (max(apply(h,1,sum))==2) {
                        res$dd=res$dd+1
                    } else if (max(apply(h,2,sum))==2) {
                        res$ds=res$ds+1
                    } else {
                        res$pa=res$pa+1
                    }
                } else if (sh == 3) {
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
#' \name{hgraph$layout}
#' \alias{hgraph$layout}
#' \alias{hgraph_layout}
#' \title{calculate a plot layout for an adjacency matrix }
#' \usage{`hgraph$layout(x,mode='sam', noise=FALSE, star.center=NULL, interactive=FALSE)`}
#' \description{
#'   This function is used to create a layout for a given graph.
#'  
#'   There are a few algorithms available such as MDS based ones like 'mds' or 'sam'
#'   and circular layouts like 'circle' or 'star', furthermore there is 
#'   the possibility to use an interactive mode where the user clicks 
#'   first on a node and then on the space where the node should be moved. 
#'   This interactive mode can be finished by a right click.
#' }
#' \arguments{
#'   \item{x}{an adjacency matrix or an adjacency list}
#'   \item{mode}{either 'mds','sam', 'circle', 'star', default: 'sam'}
#'   \item{noise}{should some scatter been added to the given coordinates, default: FALSE}
#'   \item{star.center}{the node which should be used as center of the star, if not given the first node in the graph will be in the center, default: NULL}
#'   \item{interactive}{should be there an interactive clicking to mode the nodes in the layout, default: FALSE}
#' }
#' \examples{
#' A=hgraph$graph()
#' lay=hgraph$layout(A)
#' plot(lay, pch=19,col="salmon",cex=5,xlab="",ylab="",axes=FALSE)
#' text(lay,rownames(A))
#' }
#'

hgraph$layout <- function (x,mode='sam', noise=FALSE, star.center=NULL,interactive=FALSE) {
    self=hgraph
    A=x
    if (!identical(A,t(A))) {
        A=self$d2u(A)
    }
    if (ncol(A)==3 & mode %in% c("sam","mds")) {
        mode="circle"
    }
    if (mode %in% c('mds','sam')) {
        A[A==-1]=0
        A=self$.Connect(A)
        sp=self$shortest_paths(A,mode="undirected")
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
        base::plot(xy,type="n",axes=FALSE,xlab="",ylab="")
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

#' \name{hgraph$plot}
#' \alias{hgraph$plot}
#' \alias{hgraph_plot}
#' \alias{plot.graph}
#' \title{ plot an adjacency matrix representing a graph }
#' \usage{`hgraph$plot(x, layout='sam',
#'          vertex.size=1, vertex.labels=NULL, vertex.color="grey80",
#'          vertex.cex=1, vertex.pch=19,
#'          edge.color="grey40", edge.lty=1, edge.text=NULL, edge.cex=1,
#'          edge.pch=0, edge.lwd=3,arrows=TRUE,weighted=FALSE,
#'          star.center=NULL,...)`
#' }
#' \description{
#'   This function plots an adjacency matrix representing an undirected or
#'   directed graph using different layout mechanisms.
#' 
#' }
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
#'   \item{arrows}{should edges been show in case graph is directed, default: TRUE}
#'   \item{weighted}{should the edges be shown with different weights given within the adjacency matrix}
#'   \item{star.center}{for layout 'star' the central node, default: NULL}
#'   \item{\ldots}{arguments delegated to the plot function}
#' }
#' \examples{
#' A=hgraph$graph()
#' col=hgraph$colors(A)
#' hgraph$plot(A,vertex.color=col)
#' }
#'

hgraph$plot = function (x,layout='sam',
                       vertex.size=1,vertex.labels=NULL,vertex.color="grey80",vertex.cex=1,vertex.pch=19,
                        edge.color="grey40",edge.lty=1,edge.text=NULL,edge.cex=1,edge.pch=0,
                        edge.lwd=3,arrows=TRUE,weighted=FALSE,
                        star.center=NULL,...) {
    self = hgraph
    A=x
    if (is.matrix(layout) | is.data.frame(layout)) {
        if (ncol(layout) != 2) {
            stop("If a layout matrix or data frame is given two columns are required!")
        }
    } else if (layout %in% c("sam","mds","circle","star")) {
        layout=self$layout(A,mode=layout,star.center=star.center)
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
    base::plot(layout,xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="",...)
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
            if (g=="undirected" | !arrows) {
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

plot.graph=hgraph$plot

#' \name{hgraph$tokenplot}
#' \alias{hgraph$tokenplot}
#' \alias{hgraph_tokenplot}
#' \title{Barplot with default token color codes }
#' \usage{`hgraph$tokenplot(x,xlab="token ranges", ylab="proportion", ...)`}
#' \description{
#'   This function is used to create a layout for a given graph.
#'  
#'   There are a few algorithms available such as MDS based ones like 'mds' or 'sam'
#'   and circular layouts like 'circle' or 'star', furthermore there is 
#'   the possibility to use an interactive mode where the user clicks 
#'   first on a node and then on the space where the node should be moved. 
#'   This interactive mode can be finished by a right click.
#' }
#' \arguments{
#'   \item{x}{vector of token values for a set of nodes}
#'   \item{xlab}{label for the x-axis, default: "token ranges"}
#'   \item{ylab}{label for the y-axis, default: "proportion"}
#'   \item{\ldots}{other arguments, delegated to the default barplot function}
#' }
#' \examples{
#' ### could be token values from a simulation
#' vec=stats::rlnorm(1000,meanlog=1.5)
#' hgraph$tokenplot(vec)
#' }
#'

hgraph$tokenplot <- function (x,xlab="token ranges",ylab="proportion",...) {
    x[x>100]=99
    tok=cut(x,c(0,1,9,20,99),include.lowest=TRUE)
    levels(tok)=c("0..1","2..9","10..20",">20")
    barplot(prop.table(table(tok)),col=c('skyblue','grey80','salmon','red'),ylim=c(0,1),
            cex.lab=1.8,ylab=ylab,xlab=xlab,...)
}
# private functions



hgraph$.Connect = function (g) {
    self=hgraph
    A=g
    A=as.matrix(A)
    A=A+t(A)
    A[A>0]=1
    P=self$shortest_paths(A)
    if (!any(P==Inf)) {
        return(A)
    }
    comp=self$.Components(A)
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

hgraph$.Components <- function (g) {
    self=hgraph
    A=g
    A=as.matrix(A)
    A=A+t(A)
    A[A>0]=1
    comp=c()
    P=self$shortest_paths(A)
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

