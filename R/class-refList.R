#' @include utils-unity.R
#' @include class-redirectedReference.R

refList <- setRefClass("refList", contains = "redirectedReference"
    , fields = list( .list  = "list"
                   , .class = "classRepresentation"
                   , length = #< length of the list
                        function(){base::length(.list)}
                   )
    , methods = list(
          initialize = function( ...      #< arguments to make into the list
                               , list = base::list(...) #< list to convert.
                               , class   = getClass(class(in.list[[1]])) # class that this list should contain.
            ){
                if(base::length(list)){
                    stopifnot(is_homo(list))
                    .list <<- list
                } else 
                if(!missing(class) || base::length(list)){
                    if(inherits(class, 'character')) 
                        class <- getClass(character)
                    .class <<- class
                }
            }
        , set = function(i, value){
            if(length()==0 && .class@className == "")
                .class <<- getClass(class(value))
            stopifnot(inherits(value, .class@className)) 
            .list[[i]] <<- value
        }
        , get = function(i){
            .list[[i]]
        }
        , validity  = function(){
            validif( is_homo(lapply(.list, class))
                   , inherits(.list[[1]], .class) 
                   )
        }
        , show = function(){
            methods::show(.list)
        }
        , is.list = function(){T}
        , as.list = function(){return(.list)}
        )
)
setMethod("[<-", signature(x="refList"), function(x, i, ..., value){x$set(i, value)})
setMethod("[", signature(x="refList"), function(x, i, ..., drop=F){x$get(i)})
tunnel_method(refList, length)
tunnel_method(refList, show)


