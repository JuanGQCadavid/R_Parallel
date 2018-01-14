library(parallel)

                                        # This part is the work that
                                        # the wokers is gonna perfom

doichunk <- function(ichunk){
    tot <- 0
    nr <- nrow(links) #links global at worker
    for(i in ichunk){
        tmp <- links[(i+1):nr,] %*% links[i,]
        tot <- tot + sum(tmp)
    }
    tot
}

mutoutpar <- function(cls, links){
    nr <- nrow(links) # Links global at manager
    clusterExport(cls, "links")
                                        # each "Chunk" has only 1 value of i for now
    ichunks <- 1:(nr - 1)
    tots <- clusterApply(cls, ichunks, doichunk)
    Reduce(sum, tots) / nr
}

snowsim <- function(nr, nc, cls){
    links <<- matrix(sample(0:1, (nr*nc), replace=TRUE),
                     nrow=nr)
    # << make the matrix as a Object (required for snow)
    system.time(mutoutpar(cls, links))
}


                                        # Set up cluster of n_wokers wokers on
                                        # multicore machine
initmc <- function(nworkers){
    makeCluster(nworkers)
}

                                        # set up a cluster of machines specified,
                                        # one woker per machine

initcls <- function(wokers){
    makeCluster(spec=workers)
}
