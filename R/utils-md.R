{###############################################################################
# utils-md.R
# (C) Andrew Redd
# 2014-01-21
# 
# This file is part of the R package lint.
#
# lint is free software and it's distribution and use governed by the terms
# of the GNU General Public License version 3 or greater. lint is distributed 
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# http://www.gnu.org/licenses/gpl.html
# 
# DESCRIPTION
# -----------
# Markdown conversion utilities to support markdown syntax in Lint documentation
# 
# 
}###############################################################################

md2rd <- 
function( string             #< string with documentation.
        , escape.char = '\\' #< character to escape control sequences.
        )
{
    md2rd_code(md2rd_link(string))
}

md2rd_code <- 
function( string             #< string with documentation.
        )
{
    #! convert backticks to code commands.
    gsub("(?<!\\\\)`([^`]+)`", "\\\\code{\\1}", string, perl=T)
}

md2rd_link <- 
function( string             #< string with documentation.
        )
{
    #! convert angle brackets to link commands.

    # long form
    string <- gsub("(?<!\\\\)<([^>]+)>\\(([^)]*)\\)", "\\\\link[=\\2]{\\1}", string, perl=T)

    # Short form
    string <- gsub("(?<!\\\\)<([^>]+)>", "\\\\link{\\1}", string, perl=T)
    string
}



