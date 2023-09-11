source("R/simul.R")
source("R/hgraph.R")
t1=Sys.time()
mode="gain" ;# "null","landscape"
n=400
lay=simul$gridAgents(sqrt(n))
rn=sprintf("N%03i",1:n)
rownames(lay)=rn
game.prob=as.matrix(dist(lay))
gp=game.prob<3
diag(gp)=FALSE
gp[]=as.numeric(gp)
for (i in 1:3) {
    if (i == 1) {
        res=simul$season(rn,model=mode,game.prob=NULL)
    } else {
        res=simul$season(rn,token=res$token,model=mode,game.prob=NULL)
    }  
}
print(Sys.time()-t1)
print(unlist(hgraph$triads(simul$graph(res$M,mode="win"))))
print(Sys.time()-t1)
