only1 <- function(.list){
    if(is.list(.list)){
        all(.list==.list[[1]])
    } else {
        all(.list==.list[1])
    }
}
is_unity <- function(...)only1(list(...))
is_homo <- function(.list){
    classes <- lapply(.list, class)
    only1(classes)
}
is_homogeneous <- function(...)is_homo(list(...))

validif <-
function (..., mc=match.call) 
{
    #! Utilities for checking if a class is valid
    #! 
    #! @seealso 
    #! 
    n <- length(ll <- list(...))
    if (n == 0L) 
        return(invisible(TRUE))
    ch <- character(n)
    r <- sapply(ll, function(l){if(is.logical(l) & !is.na(l)) all(l) else F})
    if(all(r)) return(T)
    mc <- match.call()
    ch <- as.character(mc)[-1]
    return(sprintf(gettext("'%s' is not TRUE"), ch)[!r])
}
