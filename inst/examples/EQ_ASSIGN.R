Duncan can you help me understand what the parser is doing here?  
The parents seem to not be coming out correctly.

This is what I get.

```{r}
getParseData(parse(text="a=1"))
##    line1 col1 line2 col2 id parent     token terminal text
##  1     1    1     1    1  1      3    SYMBOL     TRUE    a
##  3     1    1     1    1  3      0      expr    FALSE     
##  2     1    2     1    2  2      0 EQ_ASSIGN     TRUE    =
##  4     1    3     1    3  4      5 NUM_CONST     TRUE    1
##  5     1    3     1    3  5      0      expr    FALSE     
```

I would expect output along the lines of:

```{r}
getParseData(parse(text="a<-1"))
##    line1 col1 line2 col2 id parent       token terminal text
##  7     1    1     1    4  7      0        expr    FALSE     
##  1     1    1     1    1  1      3      SYMBOL     TRUE    a
##  3     1    1     1    1  3      7        expr    FALSE     
##  2     1    2     1    3  2      7 LEFT_ASSIGN     TRUE   <-
##  4     1    4     1    4  4      5   NUM_CONST     TRUE    1
##  5     1    4     1    4  5      7        expr    FALSE 
```

