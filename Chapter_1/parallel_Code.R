library(parallel) # load snow and multicore ! / require(parallel)

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

    clusterExport(cls, "links") # Snow's function -> sendt Links matrix to workers

    # if we want to send local variables then we have to uses the enviroment
    # clusterExport(cls, "links",envir=environment()) -> but the variable links
    # is gonna be global at the workers level
                                        # each "Chunk" has only 1 value of i for now
    ichunks <- 1:(nr - 1)
    tots <- clusterApply(cls, ichunks, doichunk) # Snow's function -> direct the workers
                                        # to perform their assigned chunks of work
    
    Reduce(sum, tots) / nr # R's function -> combine the results returned by the workers
}

snowsim <- function(nr, nc, cls){
    links <<- matrix(sample(0:1, (nr*nc), replace=TRUE),
                     nrow=nr)
    # << make the matrix as a global Object (required for snow)
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

#clusterApply calls fun on the first cluster node with arguments
#seq[[1]] and ... , on the second node with seq[[2]] and ...
#, and so on. If the length of seq is greater than the number of nodes in
#the cluster then cluster nodes are recycled.  A list of the results is returned;
#the length of the result list will equal the length of seq.
