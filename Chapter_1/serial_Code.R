mutoutser <- function(links){
    nr <- nrow(links)
    nc <- ncol(links)
    
    tot <- 0

    for(i in 1:(nr-1)){
        for(j in (i+1):nr){
            for(k in 1:nc){
                tot <- tot + links[i,k] * links[j,k]
            }
        }
    }

    tot / (nr * (nr-1)/2)
}

sim <- function(nr,nc){
    link <- matrix(sample(0:1, (nr*nc), replace=TRUE),
                  nrow=nr)
    system.time(mutoutser(link))
}
