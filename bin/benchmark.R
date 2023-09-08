source("R/simul.R")
source("R/hgraph.R")
t1=Sys.time()
mode="random"
for (i in 1:10) {
    if (i == 1) {
        res=simul$season(LETTERS[1:20],model=mode)
    } else {
        res=simul$season(LETTERS[1:20],token=res$token,model=mode)
    }  
}
print(unlist(hgraph$triads(simul$graph(res$M,mode="win"))))
print(Sys.time()-t1)
