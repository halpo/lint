# conversion examples
library(parser)
library(stringr)
text <- "
hw <- function(){
my.msg <- 
'Hello
World'
cat(my.msg, '\n')
}
"
lines <- readLines(textConnection(text))
(p <- attr(parser(text=text), 'data'))
(l <- str_locate('Hello', string=lines))

(f <- locate2find(l))
(r <- find2replace(f))

(s <- lint:::find_string(parse.data=p))
parse2find(s)
