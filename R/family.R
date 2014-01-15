{############################################################################### 
# family.R
# This file is part of the R package lint.
# 
# Copyright 2012 Andrew Redd
# Date: 6/16/2012
# 
# DESCRIPTION
# ===========
# family manipulation functions.
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
#' @include conversion.R

get_child_ids <- 
function( id                    #< id of the root/parent node.
        , parse.data            #< parse data information
        , nlevels =  Inf        #< number of levels to descend.
        , include.parent = TRUE #< should the root/parent node be included?
        , all.generations= include.parent #< should All generations(TRUE) or only the 
                                #^ the final (FALSE) generation be returned?
) {
#!  Get all nodes that are children of `id`.
#!  @export
  parents <- id
  ids <- if(include.parent) parents else integer(0)
  while(nlevels != 0) {
    nlevels <- nlevels - 1
    old.ids <- ids
    new.ids <- parse.data[parse.data$parent %in% parents, 'id']
    parents <- 
    ids <- unique(c(if(all.generations)ids , new.ids))
    
    if (identical(ids, old.ids)) break 
  }
  ids
}
get_child <- function(id, parse.data, ...) {
#!  @rdname get_children
#!  @export
  parse.data[parse.data$id %in% get_child_ids(id=id, parse.data, ...), ]
}
get_children <- function(id, parse.data, nlevels = - 1L){
#! Find the children of an expression
#! 
#!  This takes the \code{parse.data} and find all the children of the expression
#!  with the given \code{id}.
#! 
#!  @param id the id of the given expression in \code{parse.data}
#!  @param parse.data the data from a parsed file or expression.  
#!    The results of \code{\link{getParseData}}.
#!  @param nlevels the number of levels to search.  If a negative number is 
#!    given all children will be found.
#! 
#! @family  parse-functions
#! @export
  if (inherits(id, 'data.frame') && 'id' %in% names(id))
    id <- id$id
  stopifnot(inherits(id, 'integer'))
  return(alply(id, 1, get_child, parse.data=parse.data, nlevels=nlevels))
}
find_children <- function(...){
#'  @rdname get_children
#'  @export
  parse2find(get_children(...))
}
get_parent <- function(id, parse.data) {
    parse.data[match(id, parse.data$id), 'parent']
}
get_ancestors <- function(id, parse.data, nancestors = Inf, aggregate=TRUE){
  ids <- id
  parents <- integer(0)
  while(nancestors > 0L){
    nancestors <- nancestors - 1
    nparents <- length(parents)
    
    parents <- unique(get_parent(parents, parse.data))
    if(nparents == length(parents)) break
    ids <- if(aggregate) union(ids, parents)
  }
  return(parents)
}
get_family <- function(id, parse.data, nancestors = 0L, nchildren = Inf){
  parents <- get_ancestors(id, parse.data, nancestors = nancestors, F)
  get_child(parents, parse.data, nchildren + nancestors)
}

all_root_nodes <- function(pd                  #< parse data from `<getParseData>`
                          , recurse.groups = T #< descend into grouped code \code{\{\}}?
                          , group = 0          #< the grouping node id, used for recursion
){
#! Find all root node from parse data
#! 
#! A root node in a file is a standalone expression, such as in 
#! source file a function definition.
  roots <- subset(pd, pd$parent == group)
  if(recurse.groups) {
    groups <- is_grouping(roots$id, pd)
    if(any(groups)) {
      subs <- ldply(roots$id[groups], all_root_nodes, pd=pd
                   , recurse.groups = recurse.groups)
      rbind(roots[!groups,]
          , subset(subs, subs$token == 'expr'))
    } else roots
  } else roots
  #! @return parse data with for the root nodes.
}

is_doc_comment <- function(pd.row){
  #' Check if a row represent a comment
  #' @param pd.row row of parse data
  pd.row$token == "ROXYGEN_COMMENT"
}
get_all_doc_comments <- function(pd){
  subset(pd, pd$token == "ROXYGEN_COMMENT")
}
is_comment <- function(pd.row, allow.roxygen = F){
  #' check if a row is a comment
  #' @param pd.row row of parse.data
  #' @param allow.roxygen should roxygen 
  if (nrow(pd.row) > 1) 
    return(unlist(llply(mlply(pd.row, data.frame), is_comment
                       , allow.roxygen = allow.roxygen)))
  pd.row$token == "COMMENT" || 
  (allow.roxygen && pd.row$token == "ROXYGEN_COMMENT")
}

is_root <- function(.id = pd$id, pd){
#' test if a node is a root node
  #' @inheritParams is_grouping
  #' @description
  #' A root node is defined to be a node that either has no parent 
  #' or whose parent is a grouping node.
  if(length(.id) > 1) 
    return(laply(.id, is_root, pd))
  pd.row <- pd[pd$id == .id, ]
  if (pd.row[,'token'] != 'expr') return(FALSE)
  parent <- pd.row[,'parent']
  if (parent == 0 ) return(TRUE)
  if (is_grouping(parent, pd)) return(TRUE)
  return(FALSE)
}
ascend_to_root <- function(row = pd, pd) {
  if (nrow(row) > 1) return(daply(row, 'id', ascend_to_root, pd))
  if(is_root(row$id, pd)) return(row$id)
  parent <- row$parent
  if(parent < 0) return( - parent)
  while(!is_root(parent, pd)) 
    parent <- get_parent(parent, pd)
  return(parent)
  #' @return the parent id for the row.
}


is_grouping <- function(id = pd$id, pd){
#' test if an id is a grouping element
  #' @param id id number in \code{pd}
  #' @param pd parse data to use to check \code{id}
  if(length(id) > 1) return(laply(id, is_grouping, pd))
  row <- pd[pd$id == id, ]
  child <- get_child(id, pd, 1)
  return(  nrow(child)
        && child$token[1] == "'{'"
        && (row$parent == 0 || is_grouping(row$parent, pd)))
  #' @return a logical indicating if the root node(s) is a grouping node or not
}
get_groupings <- function(pd) {
  pd[is_grouping(pd=pd), ]
}


