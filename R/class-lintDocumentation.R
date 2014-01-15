#! @include class-refList.R

setRefClass("lintDocumentation", contains=c("redirectedReference", "VIRTUAL")
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
tunnel_method("lintDocumentation", show)

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
setRefClass( "argumentsDocumentation", contains = c("refList")
    , fields = list()
    , methods = list(
          initialize = function(..., .list = list(...)){
            force(.list)
            callSuper(list=.list, class = arg$def)
        }
        , get = function(i){arg.list[[i]]}
        , show = function(){
            lapply(.list, use_method("format_Rd"))
        }
    )            
)


    
functionDocumentation <- 
setRefClass("functionDocumentation", contains="lintDocumentation"
    , fields = list(
          title       = 'character'
        , description = 'character'
        , args        = 'argumentsDocumentation'
        , return      = 'character'
        )
    )
