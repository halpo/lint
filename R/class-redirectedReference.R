
carry_forward_args <- function(formals #< formal arguments of a function call
){
    #! make a list of arguments for passing arguments through a function.
    if(length(formals)==0)return(NULL)
    formals <- sapply(names(formals), as.name)
    formals[names(formals)=='...'] <- alist(...=)
    formals
}

tunnel_method <- function( class   #< (refClassRepresentation) class representation.
                         , method  #< (character) method
                         ){
    if(inherits(class, 'character')) class <- getRefClass(class)
    stopifnot(inherits(class, "refObjectGenerator"))
    #! call a reference class method by traditional calling
    if(inherits(method, 'function')){
        dflt <- method
        method <- deparse(substitute(method))
    } else {
        dflt <- try(getFunction(method),T)
    }
    if(method %in% class$methods()){
        class.method <- get(method, envir = class$def@refMethods)
        if(all(names(formals(class.method)) %in% names(formals(class.method)))){
            tunneling_fun <- function(){..call..}
            formals(tunneling_fun) <- formals(args(dflt))
            body(tunneling_fun) <- 
            do.call(substitute
                , list( 
                      expr = body(tunneling_fun)
                    , env  = list(..call.. = as.call(c(
                        as.call(c( as.name("$")
                            , as.name(names(formals(tunneling_fun)[1]))
                            , as.name(method)
                            ))
                        , carry_forward_args(formals(tunneling_fun)[-1])
                        ))
                    ))
                )
        } else stop(sprintf("Formal argument mismatch in method `%s` for class `%s`.", method, class))
    } else 
    if(method %in% names(fields <- class$fields())){
        # TODO program passing to a field
        tunneling_fun <- function(){..call..}
        formals(tunneling_fun) <- formals(args(dflt))
        body(tunneling_fun) <- 
        do.call(substitute
            , list( 
                  expr = body(tunneling_fun)
                , env  = list(..call.. = as.call(c( as.name("$")
                        , as.name(names(formals(tunneling_fun)[1]))
                        , as.name(method)
                        ))
                ))
            )    
    } else stop(sprintf("reference method `%s` not found in class `%s`", method, class))
    environment(tunneling_fun) <- topenv()
    setMethod(method, class$className, tunneling_fun)
}
redirectedReference <- 
setRefClass("redirectedReference", contains="VIRTUAL"
    , validity = function(object){object$validity}
)

