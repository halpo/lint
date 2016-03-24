########################################################################
# lintDocumentationClass.R
# Andrew Redd
# (c) 2015-02-17
#
# DESCRIPTION
# ===========
# Base class for documentation objects.
#
########################################################################
lintDocumentationClass <-
R6Class("lintDocumentation"
    , public = list(
          print = function( format=c('Rd', 'text') #< format print.
                          , file=NULL #< file to print to should. console by default.
            ){
                #! Prints documentation to an Rd file.
                format <- match.arg(format)
                if(format == 'Rd')
                    self$print_Rd(file=file)
                else if(format == 'text')
                    self$print_text(file=file)
                else self$print(format=format, file=file)
            }
        )
    )
