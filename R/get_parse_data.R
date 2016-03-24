{############################################################################### 
# get_parse_data.R
# This file is part of the R package lint.
# 
# Copyright 2015 Andrew Redd
# Date: 10/21/2015
# 
# LICENSE
# ========
# lint is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software 
# Foundation, either version 3 of the License, or (at your option) any later 
# version.
# 
# lint is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with 
# this program. If not, see http://www.gnu.org/licenses/.
# 
}###############################################################################

is_section <- 
function( pd #< parse data
        ){
    #! test if a parse data set consitutes a section marked in braces.
    if( pd[1,'token'] != 'expr' | pd[1,'parent'] != 0 ){
        return(FALSE)
    } else {
        x <- pd[pd[,'parent'] == pd[1,'id'],]
        x[x$id == min(x$id), 'token'] =="'{'"
    }
    #! @return a single boolean value.
}
if(F){#! @testthat
nested.text <- 
"{# Section Block
#' Roxygen Line Beore
nested <- 
function(x){
    #' line inside
    cat(\"hello world\")
}
}
"
.p <- parse(text=nested.text)
pd <- getParseData(.p)
expect_true(is_section(pd))


}

unsectionize <-
function(pd #< parse data
        ){
    #' reparse the contents of a section.
    #' 
    #' sections are denoted with curly braces
    #' 
    #' @seealso `<is_section>`
    if(is_section(pd)){
        section.text <- getParseText(pd, id=all_root_nodes(pd)$id )
        str_split(section.text, "\\n")
        
        internal.text <- 
            paste0( paste(rep(' ', pd[1,'col1']), collapse='')
                  , substr(section.text, 2, nchar(section.text)-1)
                  )
        .p <- parse(text=internal.text)
        mutate( getParseData(.p)
              , line1 = line1+pd[1, 'line1'] -1
              , line2 = line2+pd[1, 'line1'] -1
              )
    } else {
        pd
    }
    #' @return corrected parse data for the text inside a section.
}
if(F){#! @testthat unsectionize
require('dplyr')
require('testthat')
section.text <- 
"

    {# Section Block
    #' Roxygen Line Beore
    nested <- 
    function(x){
        #' line inside
        cat(\"hello world\")
    }
    }
"
.p <- parse(text = section.text)
getSrcFilename(.p)
pd <- getParseData(.p)
upd <- unsectionize(pd)

removed <- anti_join( pd, upd, by=setdiff(names(pd), c('parent', 'id'))) %>%
    arrange(line1, col1, line2, col2)
expect_equal(removed$text, c('{', '', '}'))

}

get_parse_data <-
function( x   #< object or function to retrive parse data for.
        , ... #< passed to `<getParseData>`
        ){
    #! customized version of `<getParseData>` that will return parse data for text objects.
    #! @internal
    #TODO: add filename to data before it is exported.
    pd <- getParseData(x)

    if(  is.null(pd)&& isGeneric(fdef=x) ){
        dflt <- attr(x, 'default')
        src <- getSrcref(dflt)
        if(!is.null(src)){
            x  <- dflt
            pd <- getParseData(src) 
            expr.pd <- 
                subset(pd , ( line1 > src[1] | (line1 == src[1] & col1 >= src[2]))
                          & ( line2 < src[3] | (line2 == src[3] & col2 <= src[4]))
                          )
            root <- unique(ascend_to_root( expr.pd, pd))
            return(structure( classify_comment(expr.pd)
                            , class=c("parse-data", "data.frame")
                            , header.pd = pd[pd$parent %in% -root, ]
                            ))
        }
    }
    
    if(is.null(pd)){
        if( is.null(srcref <- getSrcref(x)) )
            stop("x does not have srcref.")
        else {
            pd <- classify_comment(getParseData(parse(text=as.character(srcref))))
            return(structure(classify_comment(pd), class=c("parse-data", "data.frame")))
        }
    }
    pd <- fix_eq_assign(pd)
    switch( mode(x)
          , 'function' = {
                i <- ( pd$line1 == getSrcLocation(x, 'line'  , TRUE )
                     & pd$col1  == getSrcLocation(x, 'column', TRUE )
                     )
                parent.id <- max(pd[i, 'parent'])
                expr.pd <- get_family(parent.id, pd)
                 
                root <- unique(ascend_to_root( pd[which(i),], pd))
                root.pd <- get_family(root, pd)
                if(is_section(root.pd)){
                    pd   <- unsectionize(root.pd)
                    merged.pd <- merge(expr.pd, pd, by=setdiff(names(pd), c('id', 'parent')), suffix=c('.old', ''))
                    parent.id <- merged.pd[merged.pd$id.old == parent.id, 'id']
                    expr.pd <- merged.pd[, names(expr.pd)]
                }
                return(
                    structure( classify_comment(expr.pd)
                             , class=c("parse-data", "data.frame")
                             , header.pd = pd[pd$parent == -parent.id, ]
                             )
                      )
          }
          , return(structure(classify_comment(pd), class=c("parse-data", "data.frame")))
          )
}
if(F){# @testthat get_parse_data
test.text <- 
"#' Roxygen Line Beore
hw <- 
function(x){
    #' line inside
    cat(\"hello world\")
}"
eval(parse(text=test.text))
fun <- hw
pd <- get_parse_data(hw)
expect_that(pd, is_a("data.frame"))
expect_that(pd[1,"text"], equals("#' Roxygen Line Beore"))


nested.text <- 
"{# Section Block
#' Roxygen Line Beore
nested <- 
function(x){
    #' line inside
    cat(\"hello world\")
}
}
"
eval(parse(text=nested.text))
x <- fun <- nested
pd <- get_parse_data(nested)
expect_that(pd, is_a("data.frame"))

pd <- get_parse_data(function(){})
expect_that(pd, is_a("data.frame"))

get_parse_data(format_md)

}


fix_eq_assign <- 
function( pd  #< The [parse-data] to fix
        ){
    #! Fix the parents for expressions associated with EQ_ASSIGN tokens.
    ids <- pd[pd[['token']] == "EQ_ASSIGN", 'id']
    
    for(id in rev(ids)){
        parent <- get_parent(id, pd)
        fam.pd <- get_family(id, pd, 1, 0)
        fam.pd <- fam.pd[order(fam.pd$id), ]
        fam.pd <- head(fam.pd[fam.pd$id >= id, ], 3)
        
        new.id <- max(pd$id)+1L
        fam.pd$parent <- new.id

        line1   = min(fam.pd$line1)
        col1    = min(fam.pd[fam.pd$line1==line1, 'col1'])
        line2   = max(fam.pd$line2)
        col2    = max(fam.pd[fam.pd$line2==line2, 'col2'])
        
        pd <- 
        rbind( pd[!(pd$id %in% c(fam.pd$id)), ]
             , data.frame( line1, col1
                         , line2, col2
                         , id      = new.id
                         , parent  = parent
                         , token   = 'expr'
                         , terminal= FALSE
                         , text    = ''
                         )
             , fam.pd
             )
    }
    pd[do.call(order, pd), ]
}
if(F){#! @testthat fix_eq_assign
pd <- getParseData(parse(text="a=1"))
fixed.pd <- fix_eq_assign(pd)
expect_true(nrow(pd)+1 == nrow(fixed.pd))
expect_that(sum(fixed.pd$parent==0), equals(1))

pd <- getParseData(parse(text="a=1\nb<-2\nc=3\nd<<-4"))
fixed.pd <- fix_eq_assign(pd)
expect_true(nrow(pd)+2 == nrow(fixed.pd))
expect_that(sum(fixed.pd$parent==0), equals(4))

pd <- getParseData(parse(text="a=b=1"))
fixed.pd <- fix_eq_assign(pd)
expect_true(nrow(pd)+2 == nrow(fixed.pd))
expect_that(sum(fixed.pd$parent==0), equals(1))
}




