{############################################################################### 
# spacing.patterns.R
# This file is part of the R package lint.
# 
# Copyright 2012 Andrew Redd
# Date: 6/16/2012
# 
# DESCRIPTION
# ===========
# predefined spacing patterns.
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

#' @rdname stylechecks
#' @export
spacing.linelength.80 <- list(pattern = "^.{80}\\s*[^\\s]"
  , message = "Line width exceeds 80 characters"
  , use.lines = TRUE
  , exclude.region = .no.exclude
)

spacing.linelength.80.testinfo <- {list(
    lines = c(
        paste0(rep.int(81,'#'))
}

#' @rdname stylechecks
#' @export
spacing.linelength.100 <- list(pattern = "^.{100}\\s*[^\\s]"
  , message = "Line width exceeds 80 characters"
  , use.lines = TRUE
  , exclude.region = .no.exclude
  , warning = TRUE
)

#' @rdname stylechecks
#' @export
spacing.indentation.notabs <- list(pattern ="^\\t"
  , message = "tabs not allowed for intendation"
  , exclude.region = .no.exclude
)

#' @rdname stylechecks
#' @export
spacing.notabs <- list(pattern = "\\t"
  , message = "tabs not ever allowed"
  , exclude.region = "find_string"
)

#' @rdname stylechecks
#' @export
spacing.indentation.evenindent <- list(pattern = "^(  )*( )\\S"
  , message = "indentation should be by two spaces."
  , exclude.region = c("find_function_args", "find_call_args")
)

#' @rdname stylechecks
#' @export
spacing.spaceaftercomma <- list(pattern = ",[^\\s]"
  , message =  "no space after comma")

#' @rdname stylechecks
#' @export
spacing.spacearoundinfix <- list(
    pattern = c(paste(no.lead.rx, '(', any.opp.rx, ')', sep='')
              , paste('(', any.opp.rx, ')', no.lead.rx, sep=''))
  , message = "needs space around infix opperators"
  , exclude.region = c("find_comment", "find_string")
)

#' @rdname stylechecks
#' @export
spacing.spacearoundequals <- list(
    pattern = c(paste(no.lead.rx, '(=)(?![=])', sep='')
              , paste('(?<![=!<>])(=)', no.lead.rx, sep=''))
  , message = "needs space around infix opperators"
  , exclude.region = c("find_call_args", "find_comment", "find_string")
)

#' @rdname stylechecks
#' @export
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




