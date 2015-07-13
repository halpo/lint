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
        , format_Rd  = function(){}
        , format_text= function(){}
        , format     = function(mode = 'Rd'){
            if(mode == 'Rd') .self$format_Rd()
            else if(mode == 'text') .self$format_text()
            else stop("unknown mode")
        }
        , print      = function(mode='Rd', ...){
            base::print(.self$format(mode=mode), ...)
        }
        , show       = function(){.self$print(format = "text", file = "")}
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
            md2rd(sprintf("\\item{%s}{%s}", name, description))
        }
        , print_Rd = function(file = ""){
            #' Print R Documentation information
            cat(str_wrap(format_Rd()), file=file)
            invisible(.self)
        }
        , show = function(){print_Rd()}
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
        , format_Rd = function(){
            #! Format for R Documentation.
            items = lapply(.list, use_method("format_Rd"))
            paste( "\\arguments{\n"
                 , paste("\t", items, '\n', collapse = '')
                 , "}\n"
                 )
            #! @return A character of length 1.
        }
        , show = function(){
            cat(format_Rd())
            invisible(.self)
        }
    )
)


    
functionDocumentation <- 
setRefClass("functionDocumentation", contains="lintDocumentation"
    , fields = list(
          name        = 'character'
        , alias       = 'character'
        , title       = 'character'
        , description = 'character'
        , args        = 'argumentsDocumentation'
        , seealso     = 'character'
        , return      = 'character'
        )
    , methods = list(
          format_Rd = function(){
            #!TODO
        }
        
    )
)
