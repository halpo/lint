#' @title Style checks
#' @name style-checks
#' @docType data
#' 
#' @aliases 
#'     lint.tests
#'     style_checks
#'     style_tests
#' 
#' @format
#' Each test can be defined in several formats and is very flexible.
#' A test consists of a names list of attributes.
#' \enumerate{
#'   \item \code{pattern} is a pcre compatible \link[base:regex]{regular
#'         expression} that is tested. Can be a character vector of expressions
#'   \item OR \code{fun} which provides a function that can evaluate the 
#'         presence of the rule see section {using functions}
#'   \item \code{message} The message to be displayed.
#'   \item \code{include.region} lists regions to restrict the search to.
#'         Can be a character vector specifying the known regions, or a list of 
#'         functions that interpret output from \code{\link{parser}}.
#'   \item \code{exclude.region=c('comments', 'string')} lists regions to 
#'         restrict the search to. Operates the sames as \code{include.region}.
#'   \item \code{use.lines=T} should the pattern be evaluated in lines (default)
#'          or as a contiguous block of text.
#'   \item \code{warning=F}
#' }
#' 
#' @section Using functions
#' lint is very flexible for checking and defining style rules.
#' independent functions can be provided that extend the capabilities 
#' for checking styl rules.  Each function must be able to accept the following
#' arguments
#' \enumerate{
#'    \item \code{file} the file name as an absulute location
#'    \item \code{lines} the lines of \code{file}.
#'    \item \code{parse.data} the parse data.
#' }
#' It is highly recommended that the function also accept extra arguments 
#' through \code{...} to preserve forward compatibility.  
#' Use parse.data, and lines wherever possible.
#' 
#' The function must return either properly formated 
#' \link[parse2find]{find data}, TRUE if no problems were found,
#' or FALSE, where only an error was found but no additional information 
#' provided.  Passing is checked with \code{\link{isTRUE}}.
#' Find results will filtered through regions identified by 
#' \code{exclude.region} and include.regions.
#' 
#' @include lint.R
NULL

.no.exclude <- character(0)

{# Line Length
#' @rdname style-checks
spacing.linelength.80 <- list(pattern = "^.{80}\\s*[^\\s]"
  , message = "Line width exceeds 80 characters"
  , use.lines = TRUE
  , exclude.region = .no.exclude
)

#' @rdname style-checks
spacing.linelength.100 <- list(pattern = "^.{100}\\s*[^\\s]"
  , message = "Line width exceeds 80 characters"
  , use.lines = TRUE
  , exclude.region = .no.exclude
  , warning = TRUE
)
}
{# Spacing
#' @rdname style-checks
spacing.indentation.notabs <- list(pattern ="^\\t"
  , message = "tabs not allowed for intendation"
  , exclude.region = .no.exclude
)

#' @rdname style-checks
spacing.notabs <- list(pattern = "\\t"
  , message = "tabs not ever allowed"
  , exclude.region = "string"
)

#' @rdname style-checks
spacing.indentation.evenindent <- list(pattern = "^(  )*( )\\S"
  , message = "indentation should be by two spaces."
  , exclude.region = c("function_args", "call_args")
)

#' @rdname style-checks
spacing.spaceaftercomma <- list(pattern = ",[^\\s]"
  , message =  "no space after comma")
spacing.twobeforecomments <- {list(
    pattern = "^[^#]*[^\\s#]\\s{0,1}#"
  , exclude.region = .no.exclude)
}
}
{# infix spacing
#' @rdname style-checks
{# operator classes
escaped.opp <- c('+'='\\+', '*'='\\*', '/'='\\/', '^'='\\^')
nonesc.opp  <- c('-', '<', '>')
base.opp <- c(escaped.opp, nonesc.opp)
extended.opp <- c('\\*\\*')
logical.opp <- c('\\|', '\\|\\|', '&', '&&', '<=', '==', '!=', '>=')
assign.opp  <- c('<-', '->', '<<-', '->>')
special.opp <- c('%[^%]*%')
all.opp    <- c(base.opp, extended.opp, logical.opp, assign.opp, special.opp)
no.lead.rx = "[^\\s!%\\-\\+\\*\\/\\^<>=\\|&]"
any.opp.rx <- paste(all.opp[order(desc(str_length(all.opp)))], collapse='|')
}
{spacing.spacearoundinfix <- list(
    pattern = c(paste(no.lead.rx, '(', any.opp.rx, ')', sep='')
              , paste('(', any.opp.rx, ')', no.lead.rx, sep=''))
  , message = "needs space around infix opperators"
  , exclude.region = c("comment", "string")
)
}
{spacing.spacearoundequals <- list(
    pattern = c(paste(no.lead.rx, '(=)(?![=])', sep='')
              , paste('(?<![=!<>])(=)', no.lead.rx, sep=''))
  , message = "needs space around infix opperators"
  , exclude.region = c("call_args", "comment", "string")
)
}
}
{# Assignment
{assign.noeqassign <- list(
  fun = function(parse.data, ...){
    finds <- subset(parse.data, token.desc == "EQ_ASSIGN")
    if (nrow(finds) >= 1) return(finds[, names(empty.find)])
    return(TRUE)
  }
  , message = "'=' not allowed for assignment, use '<-'"
  , exclude.region = .no.exclude)
}
{assign.norightassign <- list(
  fun = function(parse.data, ...){
    finds <- subset(parse.data, token.desc == "RIGHT_ASSIGN")
    if (nrow(finds) >= 1) return(finds[, names(empty.find)])
    return(TRUE)
  }
  , message = "Right assignment '->' not allowed")
}
{assign.nodoubleassign <- list(
  fun = function(parse.data, ...){
    finds <- subset(parse.data, text %in% c("<<-", "->>"))
    if (nrow(finds) >= 1) return(finds[, names(empty.find)])
    return(TRUE)
  }
  , message = "Scope breaking assignment '<<-' not allowed")
}
}

#' @rdname style-checks
lint.tests <- list(
    spacing.linelength.80          = spacing.linelength.80
  , spacing.linelength.100         = spacing.linelength.100
  , spacing.notabs                 = spacing.notabs
  , spacing.indentation.evenindent = spacing.indentation.evenindent
  , spacing.spaceaftercomma        = spacing.spaceaftercomma
  , spacing.twobeforecomments      = spacing.twobeforecomments
  , spacing.spacearoundinfix       = spacing.spacearoundinfix
  , spacing.spacearoundequals      = spacing.spacearoundequals
)



