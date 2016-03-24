rewrap <- 
function( x     #< [character] text.
        , width #< width to wrap to 
        ){
    #! Wrap text to width preserving empty lines as breaks.
    empty.lines <- grepl("^\\s*$", x)
    lines <- 
        rbind( replicate(sum(empty.lines)+1, '', simplify=FALSE)
             , tapply(x, cumsum(empty.lines), paste, collapse=' ') %>%
               lapply(strwrap, width=width)
             ) %>% unlist
    lines
}
