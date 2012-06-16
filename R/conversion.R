{############################################################################### 
# Conversion.R
# This file is part of the R package lint.
# 
# Copyright 2012 Andrew Redd
# Date: 6/16/2012
# 
# DESCRIPTION
# ===========
# functions for conversion between the difference formats
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

#' @name conversion
#' @title Lint internal data structures
#' @keywords utils, internal
#' 
#' @section Introduction
#'  lint makes use of several functions from different packages that 
#'  store data in various different formats.  These functions provide 
#'  utilities for converting between the different formats.
#'  
#'  The formats are:
#'  \enumerate{
#'      \item parse   - data from the data atribute of a \code{\link{parser}}
#'                      object.  In parse data each element of an expression
#'                      has it's own row.
#'      \item find    - similar to parse but gives a row for each region or 
#'                      expression of interest.
#'      \item replace - for use with \code{\link[str_sub]{stringr}}.  
#'                      Uses a column structure with start and end, organized
#'                      into a matrix with a row for each line.
#'      \item locate  - results from \code{\link{str_locate}} from stringr.
#'                      same as replace for most purposes but does not include
#'                      a string.
#'  }
#'  
#'  
#'  Converts data from the results of \code{\link{parser}}
#'
#'  @section find data structure
#'   For the purposes of the data the find data consists of a single row 
#'   for each section/region that 
#'   contains the columns \code{line1}, \code{col1}, \code{byte1}, \code{line2}, 
#'   \code{col2}, and \code{byte2}, marking the beginning and end of a section.
#'   This is a condensation of the parse data which would have the same columns
#'   as well as additional columns, and a row for each expression in the region.
#'  
#'
#'  @section Replace data structure
#'   The data structure for replace data is defined as a data frame with 
#'   columns suitable for use ase arguments to str_sub.  That is it has columns
#'   \enumerate{
#'     \item \code{start}
#'     \item \code{end}
#'     \item and either \code{string} or \code{line}
#'   }
#'   where \code{string} would be preferred but line to match up with line data.
#'   \code{find2replace} uses the line, since the string is not available in the
#'   \link[parse2find]{find data}.
#'
#' @section Locate data structure
#'  locate data is defined as the matrix that comes from \code{\link{str_locate}}.
#'  It has columns
#'  \enumerate{
#'      \item \code{start}
#'      \item \end{end}
#'  }
#'  and has a row for every line.
empty.find <- {data.frame(
      'line1' = integer(0L)
    , 'col1'  = integer(0L)
    , 'byte1' = integer(0L)
    , 'line2' = integer(0L)
    , 'col2'  = integer(0L)
    , 'byte2' = integer(0L))}

#' @rdname conversion
#' @export
parse2find <- function(parse.data) {
  if (!inherits(parse.data, 'data.frame') && inherits(parse.data, 'list')) {
    return(ldply(parse.data, parse2find))
  }
  names1 <- c('line1', 'col1', 'byte1')
  names2 <- c('line2', 'col2', 'byte2')
  return(data.frame(
      parse.data[1L, names1]
    , tail(parse.data, 1L)[1L, names2]
  ))
}

#' @rdname conversion
#' @export
find2replace <- function(find.data) {
  mdply(find.data, function(line1, byte1, line2, byte2, ...){
    if (line1==line2) {
      data.frame(line=line1, start = byte1 + 1, end = byte2)
    } else {
      nlines = line2-line1
      data.frame(
        line  = c(line1:line2), 
        start = c(byte1+1, rep(1L, nlines)),
        end   = c(rep(-1L, nlines), byte2))
    }
  })[, -seq_len(ncol(find.data))]
}

#' @rdname conversion
#' @export
locate2find <- function(loc) {
    if(all(is.na(loc$start)))return(empty.find)
    if(!inherits(loc, .data.frame))
        loc <- as.data.frame(loc)
    mutate(loc
      , line1 = seq_along(start)
      , line2 = seq_along(start)
      ,  col1 = start
      , byte1 = start
      ,  col2 = end
      , byte2 = end)[!is.na(loc$start), names(empty.find)]
}


do_results_overlap <- function(x, y=x) {
  if (nrow(x) > 1) {
    x   <- mlply(x,data.frame)
    res <- llply(x, do_results_overlap, y=y)
    return(laply(res, noattr))
  }
  if (nrow(y) > 1) {
    y <- mlply(y,data.frame)
    res <- llply(y, do_results_overlap, x=x)
    return(laply(res, noattr))
  }
  if (x$line2 < y$line1) return(FALSE)
  if (x$line1 > y$line2) return(FALSE)
  x.start <- x$line1
  x.end   <- x$line2
  y.start <- y$line1
  y.end   <- y$line2
  max.byte <- max(x$byte1, x$byte2, y$byte1, y$byte2)
  if (max.byte>0) {
    x.start <- x.start + x$byte1/max.byte
    x.end   <- x.end   + x$byte2/max.byte 
    y.start <- y.start + y$byte1/max.byte
    y.end   <- y.end   + y$byte2/max.byte
  }
  if (x.start < y.start && y.start < x.end) return(TRUE)
  if (x.start < y.end   && y.end   < x.end) return(TRUE)
  if (y.start < x.start && x.start < y.end) return(TRUE)
  if (y.start < x.end   && x.end   < y.end) return(TRUE)
  return(FALSE)
}

merge_find <- function(...){
  # merge multiple find results
  # @param ... find results.
  find.results <- c(list(), ...)
  if (F) {
    parse.data=stop()
    x <- find_function_args(parse.data=parse.data)
    y <- find_call_args(parse.data=parse.data)
    x <- x[1,]
    y <- y[1,]
    
    x.idx <- overlaps[1, 1]
    y.idx <- overlaps[1, 2]
  }
  overlaps <- data.frame(which(do_results_overlap(x,y), arr.ind=T))
  names(overlaps) <- c('x.idx', 'y.idx')
  merged <- mdply(overlaps, function(x.idx, y.idx, x, y){
    x.row <- x[x.idx, ]
    y.row <- y[y.idx, ]
    data.frame(
      line1 = min(x.row$line1, y.row$line1),
       col1 = min( x.row$col1,  y.row$col1),
      byte1 = min(x.row$byte1, y.row$byte1),
      line2 = min(x.row$line2, y.row$line2),
       col2 = min( x.row$col2,  y.row$col2),
      byte2 = min(x.row$byte2, y.row$byte2))
  }, x=x, y=y)
  keep <- c('line1', 'col1', 'byte1', 'line2', 'col2', 'byte2')
  new.finds <- rbind(merged[keep],
    x[-overlaps$x.idx, keep],
    y[-overlaps$y.idx, keep])
  #' @results a single \code{\link{data.frame}} with find results where overlaps
  #'  were merged
  new.finds[do.call(order, new.finds), ]
}
