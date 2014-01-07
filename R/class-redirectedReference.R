

tunnel_method <- function( class   #< class
                         , method  #< method
                         ){
    #! call a reference class method by traditional calling
    formals(getFunction(method))
    tunneling_fun <- function(object, ...){
        method_fun <- do.call(`$`, list(object, method))
        method_fun(...)
    }
    setMethod(method, class, tunneling_fun)
}
redirectedReference <- 
setRefClass("redirectedReference", contains="VIRTUAL"
    , validity = function(object){object$validity}
)
tunnel_method("redirectedReference", "show")

