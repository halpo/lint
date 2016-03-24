########################################################################
# document_function.R
# Andrew Redd
# (c) 2015-02-17
#
# DESCRIPTION
# ===========
# Cleanup for documenting functions and what not.
#
########################################################################

get_lint_all_comments <- structure(get_lint_all_comments
    , class = c("self-documenting-function","function")
    , docs = function_documentation( name  = "get_lint_all_comments"
                                   , title = "Extract all comments."
                                   , arguments = arguments_list(parse.data = arg("parse.data", "Parse data", "data.frame"))
                                   )
    )
get_lint_relative          <- structure(get_lint_relative
    , class = c("self-documenting-function","function")
    , docs = function_documentation( name  = "get_lint_all_comments"
                                   , title = "Extract all lint relative comments."
                                   , arguments = arguments_list(parse.data = arg("parse.data", "Parse data", "data.frame"))
                                   )
    )
get_lint_continuation      <- structure(get_lint_continuation
    , class = c("self-documenting-function","function")
    , docs = function_documentation( name  = "get_lint_all_comments"
                                   , title = "Extract all lint continuation comments."
                                   , arguments = arguments_list(parse.data = arg("parse.data", "Parse data", "data.frame"))
                                   )
    )
get_lint_comments          <- structure(get_lint_comments
    , class = c("self-documenting-function","function")
    , docs = function_documentation( name  = "get_lint_all_comments"
                                   , title = "Extract all lint specific comments."
                                   , arguments = arguments_list(parse.data = arg("parse.data", "Parse data", "data.frame"))
                                   )
    )
get_lint_argument_descriptors <- structure(get_lint_argument_descriptors
    , class = c("self-documenting-function","function")
    , docs = function_documentation( name  = "get_lint_all_comments"
                                   , title = "Extract all lint relative and continuation comments"
                                   , arguments = arguments_list(parse.data = arg("parse.data", "Parse data", "data.frame"))
                                   )
    )

    
document_package()


