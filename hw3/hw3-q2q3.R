library(RBGL)
library(ggm)
library(gRbase)
library(gRain)
library(Rgraphviz)

dag <- list(~a, ~c|a, ~d|a:b, ~b, ~e|b, ~g|d:e, ~f|c:e, ~g|d:e, ~h|f:g)

dag.list <- dagList(dag)
dSep(as(dag.list, "matrix"), "c", "g", NULL)
dSep(as(dag.list, "matrix"), "c", "e", NULL)
dSep(as(dag.list, "matrix"), "c", "e", "g")
dSep(as(dag.list, "matrix"), "a", "g", c("d","e"))
dSep(as(dag.list, "matrix"), "a", "g", "d")

dCon <- function(dSep){
        if(dSep==FALSE){
                as.logical(dSep+1)
        }
        else{
                as.logical(dSep*0)
        }
}






