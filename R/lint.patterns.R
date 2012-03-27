
test<-

spacing.line_length.80 <- list(pattern = "^.{,80}\\s+([^\\s]+)"
  , use.lines = T
  , exclude.regions = character(0)
)
spacing.indentation.notabs <- list(pattern ="^\\t"
  , message = "tabs not allowed for intendation"
  , exclude.regions = character(0)
)
spacing.notabs <- list(pattern = "\\t", message = "tabs not ever allowed")
spacing.indentation.evenindent <- list(pattern = "^(  )*( )\\S"
  , message = "indentation should be by two spaces."
  , exclude.regions = c("function_args", "call_args")
)
  


lint.patterns <- list(

)




