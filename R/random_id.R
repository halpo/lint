random_id <- function(n, length=16){
    #! create a random string id.
    while(T){
        M <- matrix(sample(0:9, length*n, replace=TRUE), n,length)
        id <- unique(paste0("\u1", apply(M, 1, paste, collapse=''), "\u2"))
        if(length(id)==n) return(id)
    }
}
