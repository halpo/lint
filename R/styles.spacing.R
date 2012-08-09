{############################################################################### 
# styles.spacing.R
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
#' @exportPattern ^spacing\\..*$
NULL


#' @rdname stylechecks
#' @export
spacing.linelength.80 <- {list(pattern = "^.{80}\\s*[^\\s]"
  , message = "Line width exceeds 80 characters"
  , use.lines = TRUE
  , exclude.region = .no.exclude
)}
.testinfo.spacing.linelength.80 <- {list(
    lines = c('123'
      , paste(rep.int('#', 80), collapse='')
      , paste(rep.int('#', 81), collapse=''))
  , results = data.frame(line1=3, col1=1, byte1=1, line2=3, col2=81, byte2=81)
)}

#' @rdname stylechecks
#' @export
spacing.linelength.100 <- {list(pattern = "^.{100}\\s*[^\\s]"
  , message = "Line width exceeds 100 characters"
  , use.lines = TRUE
  , exclude.region = .no.exclude
  , warning = TRUE
)}
.testinfo.spacing.linelength.100 <- {list(
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
.testinfo.spacing.indentation.notabs <- {list(
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
.testinfo.spacing.notabs <- {list(
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
spacing.spacearoundinfix <- {list(
    pattern = c(paste0(no.preceeding.space.rx, '(', infix.noeq, ')')
              , paste0('(', infix.noeq, ')', no.trailing.space.rx))
  , message = "needs space around infix opperators"
  , exclude.region = c("find_comment", "find_string", "find_symbol")
)}
.testinfo.spacing.spacearoundinfix <- {list(
    lines = {c( '1 + 1'
             , '1+2'
             , '1+ 2'
             , '1 +2'
             , '1-2'                  # 5
             , '1*2'
             , '1/2'
             , '1%/%2'
             , '1^2'
             , '1**2'                 # 10
             , '1%*%2'
             , '1%o%2'
             , '1%in%2'
             , 'T&F'
             , 'T&&F'                 # 15
             , 'T|F'
             , 'T||F'
             , '1==2'
             , '1>=2'
             , '1<=2'                 # 20 
             , '1> 2'
             , '1< 2'
             , '1!=2'
             , 'if(a==b)return(TRUE)'  # 24
             )}
  , results = {data.frame( line1 = 2:24
                        ,  col1 = c(rep(2, 2), 3, rep(2, 19), 5)
                        , byte1 = c(rep(2, 2), 3, rep(2, 19), 5)
                        , line2 = 2:24
                        ,  col2 = c(rep(2, 2), 3, rep(2, 3), 3, 2, 3, 3, 4, 5 
                                    , 2, 3, 2, rep(3, 4), 2, 2, 3, 6)
                        , byte2 = c(rep(2, 2), 3, rep(2, 3), 3, 2, 3, 3, 4, 5 
                                    , 2, 3, 2, rep(3, 4), 2, 2, 3, 6)
                        )}
)}

#' @rdname stylechecks
#' @export
spacing.spacearoundequals <- {list(
    pattern = c(paste0(no.preceeding.space.rx, '(?<![=!<>])(=)(?![=])')
              , paste0('(?<![=!<>])(=)(?![=])', no.trailing.space.rx))
  , message = "needs space around `=`"
  , exclude.region = c("find_call_args", "find_comment", "find_string")
)}
# .testinfo.spacing.spacearoundequals <- {list(
    # lines = c( 'a=1'
             # , 'f(a=1)'
             # , 'function(){\na=1\n}'
             # )
    # results = data.frame( line1 = c( 1, 3)
                           # col1 = c(



#' @rdname stylechecks
#' @export
spacing.twobeforecomments <- {list(
    pattern = perl("^[^#]*[^\\s#]\\s{0,1}(?<!^[{}])#")
  , exclude.region = "find_string"
  , message = "needs two spaces spacing before inline comments")
}



