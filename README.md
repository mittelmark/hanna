## hanna

R code for the paper - Hermanussen, M., Dammhahn, M., Scheffler C. & Groth. D.
Winner-loser effects improve social network efficiency between competitors with equal resource holding power.
Sci Rep 13, 14439 (2023). [https://doi.org/10.1038/s41598-023-41225-y](https://doi.org/10.1038/s41598-023-41225-y)

Installation:

```r
> install.packages("https://github.com/mittelmark/hanna/releases/download/0.2.0/hanna_0.2.0.tar.gz",
    repos=NULL)
```

Alternatively  you can install the latest  version  directly from Github using
the remotes library:

```r
> install.libarary(remotes)
> library(remotes)
> remotes::install_github("https://github.com/mittelmark/hanna")
```

Thereafter you can check the installation like this:

```r
> library(hanna)
> citation("hanna")
```

Which should display something like this:

```r
> citation("hanna")
To cite package 'hanna' in publications use:

  Detlef Groth, University of Potsdam (2023). hanna:
  Winner-looser effects for social network efficiency simulated using
  Monte Carlo simulations. R package version 0.1.

```

## Vignette

Beside of the R functions  and the manual  pages for them are three  vignettes
which you can browser by using the vignette commands:

```r
> vignette(package="hanna")
Vignettes in package 'hanna':

simul-tutorial          Example simulations for the hanna package using
                        the simul module (source, html)
figures                 Hermanussen et. al. (2023) - Paper figures
                        (source, html)
hgraph-tutorial         hgraph introduction (source, html)

> ### Let's load one vignette from this package
> vignette("figures",package="hanna")
```

## Simulation

If the installation was successful, you can run a single season giving the
agents as many tokens as there are agents in the game using the default null model like this:

```r
> library(hanna)
> res=simul$season(LETTERS[1:6])
> res$token
### $token
### A B C D E F 
### 7 8 4 5 7 5 
> res$M
### $M
###    A  B C  D  E F
### A  0 -1 1  1  0 0
### B  1  0 0  1  0 0
### C -1  0 0 -1  0 0
### D -1 -1 1  0 -1 1
### E  0  0 0  1  0 0
### F  0  0 0 -1  0 0
```

The token does represent the overall win and loosing points, so for instance A
has won 2 times and lost 1 time so it has 6+2-1 token remaining, it is 7. In
the null model win/loose changes are independent from the number of tokens. To
use other models consult the help page from `?simul_season`. More details are as
well shown in the package vignette which you can read usually by writing
`vignette('simul-tutorial')` in your R console.

If you have such a season with the embedded result matrix you can visualize
this by using the function `simul$graph` and plotting the resulting adjacency
matrix like this:

```r
> A = simul$graph(res$M,model='win')
> par(mfrow=c(1,2),mai=rep(0.4,2))  
> hgraph$plot(A)  
> cols=hgraph$colors(A)
> hgraph$plot(A,vertex.color=cols)
```

![Simulation of 1 season with null model](img/simulation-01.png)

Agents which are winning all games are shown here in red, agents loosing all
games would be shown in blue. For more examples on how to do simulations using
the other models look at the manual pages and at the package vignettes.
 
## Author and License

Author: Detlef Groth, University of Potsdam, Germany

License: MIT license, see file [LICENSE](LICENSE).

## Bugs and Suggestions

Please use the [Issues](https://github.com/mittelmark/hanna/issues) link on
top of this README on Github.
