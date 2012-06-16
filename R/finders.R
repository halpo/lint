{############################################################################### 
# finders.R
# This file is part of the R package lint.
# 
# Copyright 2012 Andrew Redd
# Date: 6/16/2012
# 
# DESCRIPTION
# ===========
# Specific region finders.
# 
# LICENSE
# ========
# lint is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software 
# Foundation, either version 3 of the License, or (at your option) any later 
# version.
# 
# dostats is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with 
# this program. If not, see http://www.gnu.org/licenses/.
# 
}###############################################################################
#' @include find-utils.R

{ # comment
find_comment <- make_class_finder(c("COMMENT", "ROXYGEN_COMMENT"))
strip_comment   <- make_stripper(find_comment)
extract_comment <- make_extractor(find_comment)
} # comment
{ # string
find_string <- make_class_finder(c("STR_CONST"))
strip_string   <- make_stripper(find_string, replace.with='""')
extract_string <- make_extractor(find_string)
} # string
{ # function
find_function_args <- function(parse.data, ...) {
  ftokens <- subset(parse.data, parse.data$token.desc=="FUNCTION")
  ddply(ftokens, "id" , function(d, ..., parse.data) {
    p <- d$parent
    function.args <- subset(parse.data, parse.data$parent == d$p & 
      !(parse.data$token.desc %in% c('expr', 'FUNCTION')))
    parse2find(function.args)
  }, parse.data = parse.data)
}
strip_function_args   <- make_stripper(find_function_args, replace.with="()")
extract_function_args <- make_extractor(find_function_args)
find_function_body <- function(file, parse.data = attr(parser(file)), ...) {
  f.nodes <- subset(parse.data, parse.data$token.desc == "FUNCTION")
  body.parents  <- ldply(get_children(f.nodes$parent, parse.data, 1), tail, 1)
  body.contents <- find_children(body.parents, parse.data)
  parse2find(body.contents)
}
strip_function_body <- make_stripper(find_function_body, replace.with="{}")
extract_function_body <- make_extractor(find_function_body)
} # function
{ # call args
get_call_args <- function(file, parse.data=attr(parser(file)), ...) {
  call.nodes <- subset(parse.data, 
    parse.data$token.desc == "SYMBOL_FUNCTION_CALL")
  llply(call.nodes$id, get_family, parse.data=parse.data, nancestors=2)
}
find_call_args <- function(file, parse.data=attr(parser(file)), ...) {
  parse2find(get_call_args(parse.data=parse.data))
}
strip_call_args   <- make_stripper(find_call_args, replace.with="")
extract_call_args <- make_extractor(extract_call_args)
} # call args


