#' @title Style checks
#' @name stylechecks
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
#'         expression} that is tested. Can be a character vector of expressions.
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
#' 
#' @exportPattern ^spacing\\.[^\\.].*
NULL

.no.exclude <- character(0)

#' @rdname stylechecks
spacing.linelength.80 <- list(pattern = "^.{80}\\s*[^\\s]"
  , message = "Line width exceeds 80 characters"
  , use.lines = TRUE
  , exclude.region = .no.exclude
)

#' @rdname stylechecks
spacing.linelength.100 <- list(pattern = "^.{100}\\s*[^\\s]"
  , message = "Line width exceeds 80 characters"
  , use.lines = TRUE
  , exclude.region = .no.exclude
  , warning = TRUE
)

#' @rdname stylechecks
spacing.indentation.notabs <- list(pattern ="^\\t"
  , message = "tabs not allowed for intendation"
  , exclude.region = .no.exclude
)

#' @rdname stylechecks
spacing.notabs <- list(pattern = "\\t"
  , message = "tabs not ever allowed"
  , exclude.region = "find_string"
)

#' @rdname stylechecks
spacing.indentation.evenindent <- list(pattern = "^(  )*( )\\S"
  , message = "indentation should be by two spaces."
  , exclude.region = c("find_function_args", "find_call_args")
)

#' @rdname stylechecks
spacing.spaceaftercomma <- list(pattern = ",[^\\s]"
  , message =  "no space after comma")

#' @rdname stylechecks
escaped.opp <- c('+'='\\+', '*'='\\*', '/'='\\/', '^'='\\^')

#' @rdname stylechecks
nonesc.opp  <- c('-', '<', '>')

#' @rdname stylechecks
base.opp <- c(escaped.opp, nonesc.opp)

#' @rdname stylechecks
extended.opp <- c('\\*\\*')

#' @rdname stylechecks
logical.opp <- c('\\|', '\\|\\|', '&', '&&', '<=', '==', '!=', '>=')

#' @rdname stylechecks
assign.opp  <- c('<-', '->', '<<-', '->>')

#' @rdname stylechecks
special.opp <- c('%[^%]*%')

#' @rdname stylechecks
all.opp    <- c(base.opp, extended.opp, logical.opp, assign.opp, special.opp)

#' @rdname stylechecks
no.lead.rx = "[^\\s!%\\-\\+\\*\\/\\^<>=\\|&]"

#' @rdname stylechecks
any.opp.rx <- paste(all.opp[order(desc(str_length(all.opp)))], collapse='|')

#' @rdname stylechecks
spacing.spacearoundinfix <- list(
    pattern = c(paste(no.lead.rx, '(', any.opp.rx, ')', sep='')
              , paste('(', any.opp.rx, ')', no.lead.rx, sep=''))
  , message = "needs space around infix opperators"
  , exclude.region = c("find_comment", "find_string")
)

#' @rdname stylechecks
spacing.spacearoundequals <- list(
    pattern = c(paste(no.lead.rx, '(=)(?![=])', sep='')
              , paste('(?<![=!<>])(=)', no.lead.rx, sep=''))
  , message = "needs space around infix opperators"
  , exclude.region = c("find_call_args", "find_comment", "find_string")
)

#' @rdname stylechecks
spacing.twobeforecomments <- list(
    pattern = "^[^#]*[^\\s#]\\s{0,1}#"
  , exclude.region = .no.exclude
  , message = "needs two spaces spacing before inline comments")


#' @rdname stylechecks
#' @export
lint.tests <- list(
    spacing.twobeforecomments
  , spacing.spacearoundequals
  , spacing.indentation.notabs
  , spacing.linelength.80
)




