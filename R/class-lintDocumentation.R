
setRefClass("lintDocumentation", contains="VIRTUAL"
    , methods = list(
          print = function( format=c('Rd', 'text') #< format print.
                          , file=NULL #< file to print to should. console by default.
            ){
                #! Prints documentation to an Rd file.
                format <- match.arg(format)
                if(format == 'Rd')
                    .self$print_Rd(file=file)
                else if(format == 'text')
                    .self$print_text(file=file)
                else .self$print(format=format, file=file)
            }
        , print_Rd   = function(file="", ...){}
        , print_text = function(file="", ...){print_Rd(file=file, ...)}
        , show = function(){print(format = "text", file = "")}
        )
    )
setMethod("show", "lintDocumentation", function(object){object$show()})

arg <- 
setRefClass("argumentDocumentation", contains = "lintDocumentation"
    , fields = list(
          name        = 'character'
        , type        = 'character'
        , default     = 'expression'
        , description = 'character'
        )
    , methods = list(
          format_Rd = function(){
            #' format for R Documentation File
            sprintf("@param\t%s\t%s", name, description)
        }
        , print_Rd = function(file = ""){
            #' Print R Documentation information
            cat(str_wrap(format_Rd()), file=file)
        }
    )
)

argList <- 
setRefClass( "argumentsDocumentation", contains = c("lintDocumentation")
    , fields = list(arg.list = "list")
    , methods = list(
        # initialize = function(...){
        
        # }
          set = function(i, x){
            stopifnot(inherits(x, 'argumentDocumentation'))
            arg.list[[i]] <<- x
        }
        , get = function(i){arg.list[[i]]}
    )            
)
# setMethod("[<-", "argumentsDocumentation"
         # , function(object, i, x){object$set(i, x)})
# setMethod("[[<-", "argumentsDocumentation"
         # , function(object, i, x){object$set(i, x)})
# setMethod("[", "argumentsDocumentation"
         # , function(object, i){object$set(i)})
# setMethod("[[", "argumentsDocumentation"
         # , function(object, i){object$set(i)})


    
functionDocumentation <- 
setRefClass("functionDocumentation", contains="lintDocumentation"
    , fields = list(
          title       = 'character'
        , description = 'character'
        , return      = 'character'
        , args        = "argumentsDocumentation"
        )
    )
