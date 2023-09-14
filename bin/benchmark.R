## This is a Rscript to evaluate new functionalities
## in the hanna package
library(MASS)
source("R/simul.R")
source("R/hgraph.R")
t1=Sys.time()
mode="gain" ;# "null","landscape", "gain"
n=81
euc=2.1
lay=simul$gridAgents(sqrt(n),sd=0.05)
rn=sprintf("N%02i",1:n)
rownames(lay)=rn
game.prob=as.matrix(dist(lay))
gp=game.prob<euc
diag(gp)=FALSE
gp[]=as.numeric(gp)
print("game.prob=NULL")

t2=Sys.time()
for (i in 1:30) {
    if (i == 1) {
        res=simul$season(rn,token=rep(5,length(rn)),model=mode,game.prob=NULL)
    } else {
        res=simul$season(rn,token=res$token,model=mode,game.prob=NULL)
    }  
}
print(Sys.time()-t2)
print("triad calculation")
t3=Sys.time()
print(unlist(hgraph$triads(simul$graph(res$M,mode="win"))))
print(Sys.time()-t3)

print("game.prob=gp")
t2=Sys.time()

results=list()
k=0
pdf("boxplot.pdf",width=16,height=9)
layout(matrix(c(1,1,2,1,1,3),nrow=2,byrow=TRUE))

hgraph$plot(gp,layout=lay,vertex.size=0.5,arrows=FALSE,edge.lwd=1);
hist(apply(gp,1,sum),main=sprintf("Matches with Euclidean limit of %0.1f",euc))

plot_summary= function (x,main=main,FUN=function(x){signif(x,3) },...) {
    plot(1,type="n",xlim=c(0.5,6.5),ylim=c(0,3),axes=FALSE,xlab="",ylab="",main=main)
    sx=summary(x)
    #rect(0.3,2,6.2,3,col="grey80")
    text(3.5,2.5,sprintf("Summary (%i)",length(x)),cex=2)
    lines(c(0.6,6.4),y=c(3,3),lwd=2)
    lines(c(0.6,6.4),y=c(2,2),lwd=2)
    lines(c(0.6,6.4),y=c(0,0),lwd=2)
    text(1:6,y=0.5,FUN(sx,...),cex=1.4)
    text(1:6,y=1.5,names(sx),cex=1.5)
}
plot_summary(apply(gp,1,sum),FUN=round,2,main="Number of neighbours")
par(mfrow=c(2,4),mai=rep(0.5,4))
for (model in c("null","gain")) {
    for (mode in c("random", "season")) {
         for (i in 1:5) {
            for (j in 1:30) {
                if (j == 1) {
                    res=simul$season(rn,token=rep(5,length(rn)),model=model,game.prob=gp,season=mode)
                } else {
                    res=simul$season(rn,token=res$token,model=model,game.prob=gp,season=mode)
                }  
                if (j %in% c(1,5,10,30)) {
                    g=simul$graph(res$M,mode="win")
                    if (i == 1) {
                        cols=rep("grey80",ncol(res$M))
                        cols[res$token>9] = "salmon"
                        cols[res$token<2] = "skyblue"
                        cols[res$token>20] = "red"
                        hgraph$plot(g,layout='sam',vertex.color=cols,vertex.size=0.5,arrows=FALSE,edge.lwd=1);
                        if (mode == "random") {
                            mtext(paste("season",j),side=3)
                        }
                        if (j == 1) {
                            mtext(mode,side=2,line=2)
                        }
                    }
                    k=k+1
                    results[[k]] = data.frame(season=j,model=model,mode=mode,t(data.frame(unlist(hgraph$triads(g)))))
                }
                
            }
        }
        mtext(model,side=3,outer=TRUE,line=-1)
    }
}    
print(Sys.time()-t2)

print(unlist(hgraph$triads(simul$graph(res$M,mode="win"))))
results2=do.call(rbind,results)
print(summary(results2))
par(mfrow=c(2,4),mai=rep(0.5,4))
for (model in c("null","gain")) {
    for (mode in c("random", "season")) {
        for (iter in c(1,5,10,30)) {
            set=results2[results2$model==model & results2$mode==mode & results2$season==iter,4:8]
            boxplot(set)
            if (mode == "random") {
                mtext(paste("season",iter),side=3)
            }
            if (iter == 1) {
                mtext(mode,side=2,line=2)
            }
        }
    }
    mtext(model,side=3,outer=TRUE,line=-1)
}

modelCheck <- function (rn,itersteps=c(1,5,10,30), n=5,layout=NULL,...) {
    results=list()
    m=matrix(c(1,4,7,10,2,5,8,11,3,6,9,12,13,14,15,16),nrow=4,byrow=TRUE)
    par(mai=c(0.1,0.1,0.7,0.1))
    layout(m)
    k=0
    for (i in 1:n) {
        for (j in 1:max(itersteps)) {
            if (j == 1) {
                res=simul$season(rn,token=rep(5,length(rn)),...)
            } else {
                res=simul$season(rn,token=res$token,...)
            }  
            if (j %in% itersteps) {
                g=simul$graph(res$M,mode="win")
                if (i == 1) {
                    par(mai=c(0.1,0.1,0.7,0.1))
                    cols=rep("grey80",ncol(res$M))
                    cols[res$token>9] = "salmon"
                    cols[res$token<2] = "skyblue"
                    cols[res$token>20] = "red"
                    hgraph$plot(g,layout='sam',vertex.color=cols,vertex.size=0.5,arrows=FALSE,edge.lwd=1);
                    mtext(paste("season",j),side=3)
                    if(is.matrix(layout)) {
                        hgraph$plot(g,layout=layout,vertex.color=cols,vertex.size=0.5,arrows=FALSE,edge.lwd=1);
                    } else {
                        hgraph$plot(g,layout="circle",vertex.color=cols,vertex.size=0.5,arrows=FALSE,edge.lwd=1);
                    }
                    par(mai=c(0.2,0.6,0.4,0.2))
                    hgraph$tokenplot(res$token,xlab="",ylab="")
                    mtext(sprintf("Gini: %0.2f",simul$gini(res$token)),side=3,line=-2)
                }
                k=k+1
                results[[k]] = data.frame(season=j,t(data.frame(unlist(hgraph$triads(g)))))
            }
                
        }
    }
    results=do.call(rbind,results)
    par(mai=c(0.6,0.6,0.6,0.2))
    for (iter in itersteps) {
        set=results[results$season==iter,2:6]
        boxplot(set)
    }
}
modelCheck(rn,model="gain",game.prob=gp,season="season",layout=lay)
mtext("Gain model - season schedule of matches gain model", side=3, outer=TRUE,line=-2)

modelCheck(rn,model="gain",game.prob=gp,season="random",layou=lay)
mtext("Gain model - random schedule of matches gain model", side=3, outer=TRUE,line=-2)

modelCheck(rn,model="null",game.prob=gp,season="random",layout=lay)
mtext("Gain model - random schedule of matches null model", side=3, outer=TRUE,line=-2)

dev.off()
