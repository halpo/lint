..vbody <- body(Vectorize(function(a){a}))
is_vectorized <- function(f){
    identical(..vbody, body(f))
}
