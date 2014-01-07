#' @include utils-unity.R

refList <- setRefClass("refList"
    , fields = list( .list    = "list"
                   , .class = "classRepresentation"
                   )
    , methods = list(
          initialize = function( ...      #< arguments to make into the list
                               , list = base::list(...) #< list to convert.
                               , class   = getClass(class(in.list[[1]])) # class that this list should contain.
            ){
                stopifnot(is_homogeneous(list))
                if(length(list)){
                    initFields( .list = list
                              , .class = class
                              )
                }
            }
        , set(i,x) = function
        )
)





