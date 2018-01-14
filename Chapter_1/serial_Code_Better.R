

mutoutser <- function(links){
    nr <- nrow(links)
    nc <- ncol(links)
    tot <- 0

    for(i in 1:(nr-1)){
        tmp <- links[(i+1):nr,] %*% links[i,]
        tot <- tot + sum(tmp)
    }
    tot / nr
}

sim <- function(nr, nc){
    links <- matrix(sample(0:1, (nr*nc),replace=TRUE),
                    nrow=nr)
    print(system.time(mutoutser(links)))
}
