#' @docType package
#' @author Andrew Redd <Andrew.Redd@hsc.utah.edu>
#' 
#' @details
#' 
#' 
#' 
#' 
#' 

library(plyr)  
library(stringr)
library(parser)

lint <- function(file, text=NULL, patterns = lint.patterns){
#' Check a source document for stylistic errors.
#' @param file a vector of file paths.
#' @param text text to check
#' @param patterns  The named list of patterns to use to check.
#' 
#' @importFrom plyr mdply
#' @family lint
#' @export  
  stopifnot(missing(file)|inherits(file, 'character'))
  if (missing(file) && !is.null(text)) {
    stopifnot(inherits(text, "character"))
    file = textConnection(text)
    on.exit(close(file))
  }
  lines <- readLines(file)
  
  maply(patterns, )
  
} 
check_pattern <- function(lines,  pattern, message=deparse(pattern), 
  warning=F, check.comments=F, ...) {
#' Check a pattern against lines
#' 
#' This is part of lint which checks lines of text for stylistic problems.
#' The pattern provided should be a Perl compatible regular expression.
#' 
#' @param lines character vector of lines of text to check, output from 
#'   \code{\link{readLines}}.
#' @param pattern perl compatible regular expression.
#' @param message message describing the problem being checked.
#' @param warning should warning be issued for messages.
#' @param check.comments should comments be checked?  If false comments are stripped 
#'                out prior to checking.
#' @return returns an integer vector of the problem lines if problems were 
#'   found. otherwise returns TRUE if the lines pass the check. 
#'   \code{\link{isTRUE}} could be useful for checking the return value.
#' @export
#' @family lint
#' @importFrom plyr laply llply
  if(!check.comments){
    lines <- strip_comments(lines)
  }
  reg.info <- gregexpr(pattern, lines, perl=T)
  problem.yn <- laply(llply(reg.info, `>`, 0), any)
  if (any(problem.yn)) {
    problem.lines <- which(problem.yn)
    message <- sprintf(message, regmatches(lines, reg.info)[problem.lines][[1]][[1]])
    str <- sprintf("Lint: %s: found on lines %s", message, 
                   paste(problem.lines, collapse=', '))
    if(warning) warning(str) else message(str)
    return(invisible(structure(problem.lines, info=reg.info[problem.lines])))
  } else {
    return(invisible(TRUE))
  }
}

parse2find <- function(parse.data){
#'  Convert parser Structured data to find structured data
#'  
#'  Converts data from the results of \code{\link{parser}}
#'  
#'  @section find data structure 
#'  For the purposes of the data the find data consists of a single row 
#'  for each section/region that 
#'  contains the columns \code{line1}, \code{col1}, \code{byte1}, \code{line2}, 
#'  \code{col2}, and \code{byte2}, marking the beginning and end of a section.
#'  This is a condensation of the parse data which would have the same columns
#'  as well as additional columns, and a row for each expression in the region.
#'  
#'  @export
#'  @keywords utils
#'  @family type-convert
#'  @family find-functions
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
find2replace <- function(find.data){
#' Convert find structured data to replace structured data
#'
#' Converts find data to data that is suitable for use with replace function.
#'
#'  @section Replace data structure
#'  The data structure for replace data is defined as a data frame with 
#'  columns suitable for use ase arguments to str_sub.  That is it has columns
#'  \enumerate{
#'    \item \code{start}
#'    \item \code{end}
#'    \item and either \code{string} or \code{line}
#'  }
#'  where \code{string} would be preferred but line to match up with line data.
#'  \code{find2replace} uses the line, since the string is not available in the
#'  \link[parse2find]{find data}.
#'
#'
#' @param find.data find structured data
#' @export
#' @keywords utils
#' @family type-convert
#'  @family find-functions
  mdply(find.data, function(line1, byte1, line2, byte2, ...){
    if (line1==line2) {
      data.frame(line=line1, start = byte1 + 1, end = byte2 + 1)
    } else {
      nlines = line2-line1
      data.frame(
        line  = c(line1:line2), 
        start = c(byte1+1, rep(1L, nlines)),
        end   = c(rep(-1L, nlines), byte2+1))
    }
  })[, -seq_len(ncol(find.data))]
}

get_child <- function(id, parse.data, nlevels=-1L) {
#'  @rdname get_children
#'  @export
  stopifnot(length(id)==1)
  ids <- id
  while(nlevels!=0) {
    nlevels <- nlevels - 1
    old.ids <- ids
    parse.sub <- subset(parse.data, parent %in% ids)
    ids <- c(id, parse.sub$id)
    if (identical(ids, old.ids)) break 
  }
  parse.sub
}
get_children <- function(id, parse.data, nlevels=-1L){
#' Find the children of an expression
#' 
#'  This takes the \code{parse.data} and find all the children of the expression 
#'  with the given \code{id}.
#' 
#'  @param id the id of the given expression in \code{parse.data}
#'  @param parse.data the data from a parsed file or expression.  
#'    The results of \code{\link{parser}}.
#'  @param nlevels the number of levels to search.  If a negative number is 
#'    given all children will be found.
#' 
#' @family  parse-functions
#' @export
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

get_docs <- function(parent.id, parse.data) {
#'  Find  All the documentation associated with an expression
#'  
#'  Rind all Roxygen documentation that is associated with an expression
#'  regardless of the location.  The traditional Roxygen documentation 
#'  must preceed a function declaration, but this allows for finding all 
#'  Roxygen comments regardless of the location.
#'  
#'  @param parent.id the id of the expression as defined by the 
#'    \code{parse.data}
#'  @param parse.data the results from \code{\link{parser}} for the file
#'    or text containg the expression of interest.
#'  
#'  @export
  if(length(parent.id)>1) alply(parent.id, get_docs, parse.data=parse.data))
  token.desc <- NULL
  kids <- get_child(parent.id, parse.data=parse.data, nlevels=-1L)
  parent.id <- c(parent.id, kids$id)
  parse.docs <- subset(parse.data,  
      token.desc == "ROXYGEN_COMMENT"
    & (parent %in% parent.id | parent %in% -parent.id)
    )
}

strip <- function(lines, replace.data){
#'  Strip a region from the text
#'  
#'  The \code{strip} fucntion removes the region defined in \code{replace.data} 
#'  from the \code{lines}
#'  
#'  @param lines the lines with the text.  Results from \code{\link{readLines}}
#'  @param replace.data replace data info.  See also \code{\link{find2replace}}
#'  
#'  @return
#'  The \code{lines} with the regions defined in replace.data removed.
#'  
#'  @family find-functions
#'  @export
  replace.data <- mutate(replace.data, string = lines[replace.data$line])
  var.names <- c('string', 'start', 'end')
  new.lines <- maply(replace.data[, var.names], `str_sub<-`, value='', .expand=F)
  lines[replace.data$line] <- new.lines
  lines
}
extract <- function(lines, replace.data) {
#' @rdname strip
#' @description 
#' The \code{extract} function does the opposite of \code{strip}, it extracts
#' the region(s) that were found, dropping everything else.
#' 
#' @export
  replace.data <- mutate(replace.data, string = lines[replace.data$line])
  var.names <- c('string', 'start', 'end')
  maply(replace.data[, var.names], `str_sub`, .expand=F)
}

stripper <- function(finder){
  function(
    lines,
    text =  paste(lines, collapse='\n'),
    file = textConnection(text), 
    parse.data = attr(parser(file),"data")
  ){
    find <- finder(parse.data = parse.data)
    strip(lines, find2replace(find))  
  }
}
extractor <- function(finder){
  function(
    lines,
    text =  paste(lines, collapse='\n'),
    file = textConnection(text), 
    parse.data = attr(parser(file),"data")
  ) {
    find <- finder(parse.data = parse.data)
    extract(lines, find2replace(find))
  }
}

find_comments <- function(parse.data) {
  comment.data <- subset(
    parse.data
    , token.desc %in% c("COMMENT", "ROXYGEN_COMMENT")
  )
}
strip_comments   <- stripper(find_comments)
extract_comments <- extractor(find_comments)

find_function_args <- function(parse.data) {
  ftokens <- subset(parse.data, token.desc=="FUNCTION")
  ddply(ftokens, "id" , function(d, ..., parse.data) {
    p <- d$parent
    function.args <- subset(parse.data, parse.data$parent == d$p & 
      !(token.desc %in% c('expr', 'FUNCTION')))
    parse2find(function.args)
  }, parse.data = parse.data)
}
strip_function_args   <- stripper(find_function_args)
extract_function_args <- extractor(find_function_args)
 
find_function_body <- function(file, parse.data = attr(parser(file))) {
  f.nodes <- subset(parse.data, token.desc == "FUNCTION")
  body.parents  <- ldply(get_children(f.nodes$parent, parse.data, 1), tail, 1)
  body.contents <- find_children(body.parents, parse.data)
  parse2find(body.contents)
}
strip_function_body <- stripper(find_function_body)
extract_function_body <- extractor(find_function_body)




if (F) { # testing code
  file <- normalizePath("lint.R")
  parse.data <- attr(parser(file),"data")
  lines <- readLines(file)
  pattern = patterns[1,1]
  message = patterns[1,2]
  check_pattern(lines, pattern, "no space after comma")
  text.block = paste(lines, collapse="\n")

  find.data    <- find_function_args(parse.data=parse.data)
  replace.data <- find2replace(find.data)
  strip(lines, replace.data)

  comments <- find_comments(file)
  replace  <- find2replace(comments)
  replace <- mutate(replace, end=-1L)
  strip(lines, replace)
  strip_comments(parse.data=parse.data) 
  strip_comments(file) 
  
  get_comments(lines, parse.data=parse.data)
  extract_comments(lines)
  
  find_function_body(parse.data)  
  
  top.expressions <- subset(parse.data, parent==0)
  expressions <- get_children(top.expressions, parse.data)

  docs <- extract_comments(lines, parse.data = expressions[[4]])
  cat(paste(docs, '\n', collapse=''))
  
  row <- top.expressions[1,]
  id  <- top.expressions$id
  get_children(id, parse.data)
  
  find_function_body(parse.data)
  
  alply(top.expressions$id, 1, find_associated, parse.data)
    
  pid=278
}
