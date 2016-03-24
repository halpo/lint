{############################################################################### 
# is_parse_data.R
# This file is part of the R package lint.
# 
# Copyright 2012 Andrew Redd
# Date: 6/16/2012
# 
# DESCRIPTION
# ===========
# testing for parse data expressions.
# 
# LICENSE
# ========
# lint is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software 
# Foundation, either version 3 of the License, or (at your option) any later 
# version.
# 
# lint is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with 
# this program. If not, see http://www.gnu.org/licenses/.
# 
}###############################################################################


is_parse_data <- 
function( df #< a [data.frame] object.
        ){
    #! Test if the given `df` data.frame conformes to `<parse-data>` conventions.
    if(inherits(df, "parse-data")) return(TRUE)
    if(!inherits(df, "data.frame")) return(FALSE)
    all(c( "line1", "line2", "col1", "col2", "id", "parent"
            , "token", "terminal", "text") %in% names(df))
}

assignment.opperators <- 
    c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")
is_pd_assignment <- 
function( pd            #< parse data of assignemnt
        , id = all_root_nodes(pd)$id[1] #< id of interest.
        ){
    #! check if the provided parse-data is an assignment expression.
    if(pd[pd$id == id, 'token'] != 'expr')
        FALSE
    kids.pd <- get_children(id=id, pd, nlevels=1)[[1]]
    kids.pd <- kids.pd[kids.pd[['token']] != 'expr', ]
    
    any(kids.pd[['token']] %in% assignment.opperators)
}
if(F){#! @testthat is_pd_assignment
    pd <- get_parse_data(parse(text="x <-  1"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="x <<- 1"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="1 ->  x"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="1 ->> x"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="x = 1"))
    expect_true(is_pd_assignment(pd))
}

get_pd_assign_value <- 
function( pd #< The [parse-data] object, representing an assignment 
        ){
    #! get the value of an assignment operator expression.
    #! 
    #! This function assumes correct structure and does not check for compliance.
    kids.pd <- sort(get_child(id=all_root_nodes(pd)$id, pd, 1, FALSE))
    switch( kids.pd[2, 'token']
          , RIGHT_ASSIGN = get_family( id = head(kids.pd$id,1), pd)
          , LEFT_ASSIGN  = get_family( id = tail(kids.pd$id,1), pd)
          , EQ_ASSIGN    = get_family( id = tail(kids.pd$id,1), pd)
          )
}
get_pd_assign_value_id <- 
function( pd #< The [parse-data] object, representing an assignment
        , id = all_root_nodes(pd)$id
        ){
    #! Get the id for the value portion of an assignment operator expression.
    if(length(id) > 1)
        sapply(id, get_pd_assign_value_id, pd=pd)
    child.ids <- get_child_ids(id=id, pd=pd, 1, FALSE)
    type <- subset(pd, id %in% child.ids & token %in% assignment.opperators)$token
    switch( type
          , RIGHT_ASSIGN = min(child.ids)
          , max(child.ids)
          )
}
if(F){#! testthat get_pd_assign_value
pd <- get_parse_data(parse(text="x<-1"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x=1"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x<<-1"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->x"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->>x"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)
}

get_pd_assign_variable <- 
function( pd #< The [parse-data] object, representing an assignment 
        ){
    #! get the value of an assignment operator expression.
    #! 
    #! This function assumes correct structure and does not check for compliance.
    kids.pd <- sort(get_child(id=all_root_nodes(pd)$id, pd, 1, FALSE))
    switch( kids.pd[2, 'token']
          , RIGHT_ASSIGN = get_family( id = tail(kids.pd$id,1), pd)
          , LEFT_ASSIGN  = get_family( id = head(kids.pd$id,1), pd)
          , EQ_ASSIGN    = get_family( id = head(kids.pd$id,1), pd)
          )
}
get_pd_assign_variable_id <- 
function( pd #< The [parse-data] object, representing an assignment
        , id = all_root_nodes(pd)$id
        ){
    #! Get the id for the variable portion of an assignment operator expression.
    if(length(id) > 1)
        sapply(id, get_pd_assign_variable_id, pd=pd)
    child.ids <- get_child_ids(id=id, pd=pd, 1, FALSE)
    type <- subset(pd, id %in% child.ids & token %in% assignment.opperators)$token
    switch( type
          , RIGHT_ASSIGN = max(child.ids)
          , min(child.ids)
          )
}


is_pd_function <-
function( pd #< a [parse-data] object
        , id = all_root_ids(pd)
        ){
    #! test if parse data is a function
    kids.pd <- get_child(id=id, pd, nlevels=1, FALSE)
    kids.pd[1, 'token'] == 'FUNCTION'
}
if(F){#! @testthat is_pd_function
    pd <- get_parse_data(parse(text="function(){}"))
    expect_true(is_pd_function(pd))

    pd <- get_parse_data(parse(text="fun <- function(){}"))
    expect_false(is_pd_function(pd))
}
