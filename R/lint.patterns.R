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

#' @include pattern-utils.R
NULL


#' @rdname stylechecks
#' @export
spacing.linelength.80 <- list(pattern = "^.{80}\\s*[^\\s]"
  , message = "Line width exceeds 80 characters"
  , use.lines = TRUE
  , exclude.region = .no.exclude
)
spacing.linelength.80.testinfo <- {list(
    lines = c('123'
      , paste(rep.int('#', 80), collapse='')
      , paste(rep.int('#', 81), collapse=''))
  , results = data.frame(line1=3, col1=1, byte1=1, line2=3, col2=81, byte2=81)
)}

#' @rdname stylechecks
#' @export
spacing.linelength.100 <- list(pattern = "^.{100}\\s*[^\\s]"
  , message = "Line width exceeds 100 characters"
  , use.lines = TRUE
  , exclude.region = .no.exclude
  , warning = TRUE
)
spacing.linelength.100.testinfo <- {list(
    lines = c('123'
      , paste(rep.int('#', 100), collapse='')
      , paste(rep.int('#', 101), collapse=''))
  , results = data.frame(line1=3, col1=1, byte1=1, line2=3, col2=101, byte2=101)
)}

#' @rdname stylechecks
#' @export
spacing.indentation.notabs <- list(pattern ="^\\t"
  , message = "tabs not allowed for intendation"
  , exclude.region = .no.exclude
)
spacing.indentation.notabs.testinfo <- {list(
    lines = c('    "hello world"'   # Good
            , '\t"hi there"'        # Bad
            , '"don\'t\t catch me"' # Good, inside string
            , '#don\'t\t catch me'  # OK, not at beginning
            , 'IM <-\twrong()')     # OK, not at beginning
  , results = data.frame(
        line1 = as.integer(c(2))
      , col1  = as.integer(c(1))
      , byte1 = as.integer(c(1))
      , line2 = as.integer(c(2))
      , col2  = as.integer(c(1))
      , byte2 = as.integer(c(1)))
)}

#' @rdname stylechecks
#' @export
spacing.notabs <- list(pattern = "\\t"
  , message = "tabs not ever allowed"
  , exclude.region = "find_string"
)
spacing.notabs.testinfo <- {list(
    lines = c('    "hello world"'   # Good
            , '\t"hi there"'        # Bad
            , '"don\'t\t catch me"' # OK, inside string(excluded)
            , "#don't\t catch me"  # Bad, inside comment, not excluded
            , 'IM <-\twrong()')     # Bad
  , results = data.frame(
        line1 = as.integer(c(2, 4, 5))
      , col1  = as.integer(c(1, 7, 6))
      , byte1 = as.integer(c(1, 7, 6))
      , line2 = as.integer(c(2, 4, 5))
      , col2  = as.integer(c(1, 7, 6))
      , byte2 = as.integer(c(1, 7, 6)))
)}

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




