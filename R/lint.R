#' @name package-lint
#' @title \code{lint}: R code style checker
#' @docType package
#' @author Andrew Redd
#' 
#' @details
#' \code{lint}
#' 
#' @import plyr
#' @import stringr
#' @importFrom parser parser
#' @importFrom harvestr noattr
#' @importFrom dostats collect
#' @include lint.patterns.R
NULL

{ # TODO
  # ----
  # * independent function as a check.
  # * 
  # 
} 

#{ # Core Functions

#' Look for an argument.
#' 
#' @param x an object
#' @param default the default value
#' @return If x is neither NULL nor NA then x otherwise the default
#' @export
with_default <- function(x, default) {
  if (all(is.null(x))) return(default)
  if (length(x) > 0 && all(is.na(x))) return(default)
  x
}

#' Check a source document for stylistic errors.
#' @param file a vector of file paths.
#' @param text text to check
#' @param tests The list of tests to use to check.
#' 
#' @family lint
#' @export
lint <- function(file, text=NULL, tests = lint.tests) {
  stopifnot(missing(file)|inherits(file, 'character'))
  if (missing(file) && !is.null(text)) {
    stopifnot(inherits(text, "character"))
    file = textConnection(text)
    on.exit(close(file))
  }
  
  parse.data=attr(parser(file), 'data')
  lines=readLines(file)
  
  llply(lint.tests, redirf(dispatch_test), file=file
        , parse.data=parse.data, lines=lines)  
}

#' Dispatch tests to the appropriate handler
#' @param test the test
#' @param file the file to check
#' @param parse.data parse data from \code{\link{parser}}
#' @param lines the lines to evaluate, overrides file.
#' @param quiet should the test be quiet, i.e. no messages or warnings?
#' @param warning should messages be upgraded to warnings, ignored if 
#'                \code{quiet=TRUE}.
#' 
#' @description
#' runs a test the the appropriate handler.
#' 
#' @return 
#' returns the results from the test handler, which should be either a TRUE for
#' a passed test or the lines, locations, and/or string violating the rules.
#' @export
dispatch_test <- function(test, file, parse.data=attr(parser(file), 'data')
  , lines=readLines(file), quiet=FALSE, warning=with_default(test$warning, FALSE)
) {
  include.region <- with_default(test$include.region, character(0))
  if (length(include.region)>=1L) {
  }
  
  exclude.region <- with_default(test$exclude.region, c("comment", "string"))
  
  if (length(exclude.region)> 0L) {
    char.ex.region.idx <- laply(exclude.region, inherits, 'character')
    fun.ex.region.idx  <- laply(exclude.region, inherits, 'function')
    if (!all(char.ex.region.idx | fun.ex.region.idx)) {
      stop("Exclude regions must be either character strings or functions that return find data.")
    }
    fun.ex.region <- as.list(exclude.region )
    ex.region.names  <- paste("find", exclude.region[char.ex.region.idx], sep='_')
    mf <- function(...)match.fun(...)
    for(i in which(char.ex.region.idx)) {
        fun.ex.region[[i]] <- try(get(ex.region.names[i], mode = 'function'
                                      , inherits=T), silent=TRUE)
        if(inherits(fun.ex.region[[i]], 'try-error'))
            fun.ex.region[[i]] <- match.fun(ex.region.names[i])
    }
    for(fun.ex in fun.ex.region){
      lines <- fun.ex(lines=lines)
    }
    
    # find method
    find.region.names  <- paste("find", exclude.region[char.ex.region.idx], sep='_')
    fun.find.region <- as.list(exclude.region)
    for(i in which(char.ex.region.idx)) {
        fun.find.region[[i]] <- try(get(ex.region.names[i], mode = 'function'
                                      , inherits=T), silent=TRUE)
        if(inherits(fun.find.region[[i]], 'try-error'))
            fun.find.region[[i]] <- match.fun(ex.region.names[i])
    }
    ex.regions <- 
        llply(fun.find.region, do.call, list(parse.data=parse.data))
    
  } 
  
  use.lines = with_default(test$use.lines, TRUE)
  if (!use.lines) lines <- paste(lines, '\n', collapse='')
  
  do_message <- if(quiet){
    function(...){}
  } else if(warning) {
    get("warning", mode="function")
  } else {
    get("message", mode="function")
  }

  if (!is.null(test$pattern)) {
    test.result <- do.call(check_pattern, append(test, list(lines=lines)))
    if(isTRUE(test.result)) return(TRUE)
    test.message <- with_default(test$message, test$pattern)
    str <- sprintf("Lint: %s: found on lines %s", test.message, 
                   paste(test.result, collapse=', '))
    do_message(str)
    return(invisible(test.result))
  } else
  stop("Ill-formatted check.")
}
   
#' Check a pattern against lines
#' 
#' This is part of lint which checks lines of text for stylistic problems.
#' The pattern provided should be a Perl compatible regular expression.
#' 
#' @param lines character vector of lines of text to check, output from 
#'   \code{\link{readLines}}.
#' @param pattern perl compatible regular expression.
#' @param ... discarded.
#' @return returns an integer vector of the problem lines if problems were 
#'   found. otherwise returns TRUE if the lines pass the check. 
#'   \code{\link{isTRUE}} could be useful for checking the return value.
#' @export
#' @family lint
check_pattern <- function(pattern
  , lines
  , ...) {
if(F){
  pattern = "^.{80}\\s*[^\\s]"
}
  if (length(pattern)>1) {
    problem.yn <- llply(pattern, grepl, lines, perl=T)
    problem.yn <- collect(problem.yn, `|`)
  } else {
    problem.yn    <- grepl(pattern, lines, perl=T)
  }
  problem.lines <- which(problem.yn)
  if (any(problem.lines)) {
    return(problem.lines)
  } else {
    return(TRUE)
  }
}
#} # Core Functions
{ # Conversion
parse2find <- function(parse.data) {
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
find2replace <- function(find.data) {
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
} # Conversion
{ # Family Functions
get_child <- function(id, parse.data, nlevels=-1L, include.parent=TRUE) {
#'  @rdname get_children
#'  @export
  stopifnot(length(id)==1)
  ids <- id
  while(nlevels!=0) {
    nlevels <- nlevels - 1
    old.ids <- ids
    parse.sub <- subset(parse.data, parse.data$parent %in% ids)
    if(include.parent) ids <- c(id, parse.sub$id) else ids <- parse.sub$id       
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
get_parent <- function(id, parse.data) {
  parse.data[parse.data$id == id, 'parent']
}
get_family <- function(id, parse.data, nancestors=0L, nchildren=Inf){
  parents <- id
  while(nancestors > 0L){
    nancestors <- nancestors -1
    nchildren <- nchildren + 1
    parents <- get_parent(parents, parse.data)
  }
  get_child(parents, parse.data, nchildren)
}
} # Family Functions
{ # strip/extract utilities
strip <- function(lines, replace.data, replace.with=''){
#'  Strip a region from the text
#'  
#'  The \code{strip} fucntion removes the region defined in \code{replace.data} 
#'  from the \code{lines}
#'  
#'  @param lines the lines with the text.  Results from \code{\link{readLines}}
#'  @param replace.data replace data info.  See also \code{\link{find2replace}}
#'  @param replace.with what to replace with , if there is a need.
#'  
#'  @return
#'  The \code{lines} with the regions defined in replace.data removed.
#'  
#'  @family find-functions
#'  @export
  if(nrow(replace.data)==0) return(lines)
  replace.data <- mutate(replace.data, string = lines[replace.data$line])
  var.names <- c('string', 'start', 'end')
  new.lines <- maply(replace.data[, var.names], `str_sub<-`, value=replace.with,
    .expand=F)
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
  if(nrow(replace.data)==0) return(lines)
  replace.data <- mutate(replace.data, string = lines[replace.data$line])
  var.names <- c('string', 'start', 'end')
  maply(replace.data[, var.names], `str_sub`, .expand=F)
}
make_stripper <- function(finder, replace.with=''){
  replace.with.default = replace.with
  function(
    lines,
    text =  paste(lines, collapse='\n'),
    file = textConnection(text), 
    parse.data = attr(parser(file),"data"),
    replace.with = replace.with.default,
    ...
  ){
    find <- finder(parse.data = parse.data)
    strip(lines, find2replace(find), replace.with=replace.with)  
  }
}
make_extractor <- function(finder){
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
} # strip/extract utilities
{ # Region Finders
{ # comment
find_comment <- function(parse.data) {
  comment.data <- subset(parse.data
    , parse.data$token.desc %in% c("COMMENT", "ROXYGEN_COMMENT")
  )
}
strip_comment   <- make_stripper(find_comment)
extract_comment <- make_extractor(find_comment)
} # comment
{ # string
find_string <- function(parse.data){
  subset(parse.data,
    parse.data$token.desc %in% c("STR_CONST")
  )
}
strip_string   <- make_stripper(find_string, replace.with='""')
extract_string <- make_extractor(find_string)
} # string
{ # function
find_function_args <- function(parse.data) {
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
find_function_body <- function(file, parse.data = attr(parser(file))) {
  f.nodes <- subset(parse.data, parse.data$token.desc == "FUNCTION")
  body.parents  <- ldply(get_children(f.nodes$parent, parse.data, 1), tail, 1)
  body.contents <- find_children(body.parents, parse.data)
  parse2find(body.contents)
}
strip_function_body <- make_stripper(find_function_body, replace.with="{}")
extract_function_body <- make_extractor(find_function_body)
} # function
{ # call args
get_call_args <- function(file, parse.data=attr(parser(file))){
  call.nodes <- subset(parse.data, 
    parse.data$token.desc == "SYMBOL_FUNCTION_CALL")
  llply(call.nodes$id, get_family, parse.data=parse.data, nancestors=2)
}
find_call_args <- function(file, parse.data=attr(parser(file))){
  parse2find(get_call_args(parse.data=parse.data))
}
strip_call_args   <- make_stripper(find_call_args, replace.with="")
extract_call_args <- make_extractor(extract_call_args)
} # call args
}# Region Finders
if (F) {  # testing code
  using(plyr, stringr, parser, harvestr, compiler)
  file <- normalizePath("lint.R")
  parse.data <- attr(parser(file),"data")
  lines <- readLines(file)
  
  get_call_args(parse.data=parse.data)
}
