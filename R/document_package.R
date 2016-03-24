########################################################################
# document_package.R
# Andrew Redd
# (c) 2015-02-17
#
# DESCRIPTION
# ===========
# Function documentation functions.
#
########################################################################
#' @import function-documentation.R


if(FALSE){# development
    options(error=recover
           , warn =2)
    env  <- asNamespace("lint")
    name <- ""
    
    traceback()
}
document_package <- 
function(){
    #! document all objects in a package.
    #! 
    #! Place this as the last command when loading the code. 
    #! This should be in a file such as `zzz.R` but should always
    #! be listed last in the Collate field of the DESCRIPTION file.
    env <- topenv()
    objects <- ls(envir=env, all=TRUE) 
    for(name in objects){
        object <- get(name, envir=env)
        cat(name,"\n", file="log.txt", append=TRUE)
        if(is.function(object)){
            if(!inherits(object, "self-documenting-function"))tryCatch({
                fun <- object
                documented.object <- 
                    document_function(object)
                assign(name, documented.object, envir=env)
            }, error=function(e){
                warning( "Error while processing documentation for `", name, "`:\n"
                       , e$message)
            })
        }
        #ENHANCEMENT: Other object documentations. 
    }
}





