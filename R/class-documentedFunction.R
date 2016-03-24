{###############################################################################
# class-DocumentedFunction.R
# Andrew Redd
# 2013-10-06
# 
# DESCRIPTION
# ===========
# functions for self documenting functions
# 
}###############################################################################
#' @include class-lintDocumentation.R
#' @include classify_comment.R

get_function_body_id <- 
function( pd                    #< parse data containing the function
        , id = all_root_ids(pd) #< id for function of interest
        ){
    #! Subset parse data to function body.
    #! 
    #! Will include the curly braces if the function contains them.
    #! Can accept an assignment with the function as the value.
    #! expects a single function.
    if(is_pd_assignment(pd, id)) id <- get_pd_assign_value_id(pd, id)
    if(!is_pd_function(pd, id))stop("id(", id, ") does not indicate a function")
    max(get_child_ids(pd=pd, id=id, 1, FALSE))
    #! @return id in `pd` pointing to the parent of the body of the function.
}
if(F){#!testthat
    pd <- get_parse_data(get_function_body_id)
    expect_true(is_pd_assignment(pd))
    body.id <- get_function_body_id(pd)
    expect_is(body.is, 'integer')
}
get_function_body_parse <- 
function( ... #< Passed onto 
        ){
    body.id <- get_function_body_id(...)
    get_family(body.id, pd)
    #! @return `<parse.data>` data.frame.
}
if(F){#!testthat get_function_body_parse
    pd <- get_parse_data(get_function_body_id)
    body.pd <- get_function_body_parse(pd)
    expect_equal(body.pd[1,'text'], "{")
}

