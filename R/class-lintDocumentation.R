
setRefClass("lintDocumentation", contains="VIRTUAL"
    , methods = list(
        print = function(file=NULL #< file to print to should. console by default.
            ){
                #! Prints documentation to an Rd file.
            }
        )
    )

arg <- 
setRefClass("argumentDocumentation", contains = "lintDocumentation"
    , fields = list(
          name        = 'numeric'
        , type        = 'numeric'
        , default     = 'expression'
        , description = 'character'
        )
    #TODO: program print.
    )

argList <- 
setRefClass("argumentsDocumentation", contains = c("lintDocumentation", "list"))
    
functionDocumentation <- 
setRefClass("functionDocumentation", contains="lintDocumentation"
    , fields = list(
          title       = 'character'
        , description = 'character'
        , return      = 'character'
        , args        = "argumentsDocumentation"
        )
    )
