{###############################################################################
# parse-data-utils.R
# This file is part of the R package lint.
#
# Copyright 2012 Andrew Redd
# Date: 6/16/2012
#
# DESCRIPTION
# ===========
# Utilities for dealing with parse-data data.frames.
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

`sort.parse-data` <-
function( x #< a [parse-data] object.
        , decreasing = FALSE  #< Decreasing?
        ){
    #! sort parse data into cannonical form.
    #!
    #! by the following:
    #!
    #! * `line1`
    #! * `col1`
    #! * decreasing `line2`
    #! * decreasing `col2`
    #! * increasing `id `
    #!
    #! The effect is that parents should always come before children.
    #!
    o <- order(x$line1, x$col1, -x$line2, -x$col2, x$id)
    if(decreasing) o <- rev(o)
    x[o, ]
}


reconstitute_expression <-
function( pd            #< A [parse-data] object
        , id = pd$id    #<
        ){
    #< convert the `pd` parse-data to an expression.
    parse(text = getParseText(pd, id=id))
}

