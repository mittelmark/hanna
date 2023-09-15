library(MASS)
source("R/simul.R")
source("R/hgraph.R")
t1=Sys.time()
pdf("model-check.pdf",width=16*0.7,height=9*0.7)
n=100
sd=0.1
d=2.1
grid=simul$gridAgents(round(sqrt(n)),sd=sd)
P=simul$d2prob(grid,mode="euc",d=d)
simul$checkLayout(grid,P)
mtext(side=3,sprintf("distance=euclidean, d=%0.1f, s=%0.1f",d,sd),outer=TRUE,cex=1.4,line=-2)
simul$checkModel(grid,season="random",model="gain",game.prob=P)
secs=as.numeric(difftime(Sys.time(), t1, units = "secs"))
mtext(side=3,sprintf("season=random, model=gain, seconds: %0.3f",secs),outer=TRUE,cex=1.4,line=-2)

dev.off()


