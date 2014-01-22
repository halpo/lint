{###############################################################################
# utils-md.R
# Andrew Redd
# 2014-01-21
# 
# 
# DESCRIPTION
# -----------
# Markdown conversion utilities to support markdown syntax in Lint documentation
# 
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



