setRefClass("lintDocumentation", contains=c("redirectedReference", "VIRTUAL")
    , methods = list(
          print = function( format=c('Rd', 'text') #< format print.
                          , file=""   #< file to print to. console by default.
                          , ...       #< Passed on to `<cat>`.
                          ){
                "Prints documentation to an Rd file."
                if(format == 'Rd')
                    cat(.self$format_Rd(), file=file, ...)
                else if(format == 'text')
                    cat(.self$format_text(), file=file, ...)
                else 
                    cat(.self$format(mode=mode), file=file, ...)
            }
        , format_Rd  = function(){}
        , format_text= function(){}
        , format     = function(mode = 'Rd'){
            "Format documentation into a single string."
            if(mode == 'Rd') .self$format_Rd()
            else if(mode == 'text') .self$format_text()
            else stop("unknown mode")
        }
        , show       = function(){.self$print(format = "text", file = "")}
        )
    )

arg <- 
setRefClass("argumentDocumentation", contains = "lintDocumentation"
    , fields = list(
          name        = 'character'
        , type        = 'character'
        , default     = 'expression'
        , description = 'character'
        )
    , methods = list(
        format_text = function(){
            #! Format for text printing.
            d <- if(!is.null(default))
                sprintf("=%s", deparse(default))
            
            sprintf("%s\t%s", name, description)
        }
        , format_Rd = function(){
            #! format for R Documentation File
            md2rd(sprintf("\\item{%s}{%s}", name, description))
        }
        , print_Rd = function(file = ""){
            #! Print R Documentation information
            cat(str_wrap(format_Rd()), file=file)
            invisible(self)
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
            invisible(self)
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
            s <- paste0(.self$title, "\n\n")
            s <- paste0(s, format(.self$args), "\n\n")
            if(!is.null(.self$description))
                s <- paste0(s, .self$description, "\n\n")
            if(!is.null(.self$seealso))
                s <- paste0(s, paste(.self$seealso, collapse=", ") , "\n\n")
            return(s)
        }
        
    )
)
