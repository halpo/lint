is_consecutive <- function(x){
    c(T, diff(x)==1)
}
first_consecutive <- function(x){
    Reduce(`&&`, is_consecutive(x), accumulate=TRUE)
}
