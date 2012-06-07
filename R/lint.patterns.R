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
#' NULL

.no.exclude <- character(0)

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

#' @rdname style-checks
escaped.opp <- c('+'='\\+', '*'='\\*', '/'='\\/', '^'='\\^')

#' @rdname style-checks
nonesc.opp  <- c('-', '<', '>')

#' @rdname style-checks
base.opp <- c(escaped.opp, nonesc.opp)

#' @rdname style-checks
extended.opp <- c('\\*\\*')

#' @rdname style-checks
logical.opp <- c('\\|', '\\|\\|', '&', '&&', '<=', '==', '!=', '>=')

#' @rdname style-checks
assign.opp  <- c('<-', '->', '<<-', '->>')

#' @rdname style-checks
special.opp <- c('%[^%]*%')

#' @rdname style-checks
all.opp    <- c(base.opp, extended.opp, logical.opp, assign.opp, special.opp)

#' @rdname style-checks
no.lead.rx = "[^\\s!%\\-\\+\\*\\/\\^<>=\\|&]"

#' @rdname style-checks
any.opp.rx <- paste(all.opp[order(desc(str_length(all.opp)))], collapse='|')

#' @rdname style-checks
spacing.spacearoundinfix <- list(
    pattern = c(paste(no.lead.rx, '(', any.opp.rx, ')', sep='')
              , paste('(', any.opp.rx, ')', no.lead.rx, sep=''))
  , message = "needs space around infix opperators"
  , exclude.region = c("comment", "string")
)

#' @rdname style-checks
spacing.spacearoundequals <- list(
    pattern = c(paste(no.lead.rx, '(=)(?![=])', sep='')
              , paste('(?<![=!<>])(=)', no.lead.rx, sep=''))
  , message = "needs space around infix opperators"
  , exclude.region = c("call_args", "comment", "string")
)

#' @rdname style-checks
spacing.twobeforecomments <- list(
    pattern = "^[^#]*[^\\s#]\\s{0,1}#"
  , exclude.region = .no.exclude)


#' @rdname style-checks
lint.tests <- list(

)




