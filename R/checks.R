
# all checks return either TRUE or 
# one or more error messages. 

# In checker, the error message is created by evaluating
# err in the context of the input x and other inputs vals.

checker <- function(expr, errmsg, error=stop) {
  expr_sub <- eval(substitute(substitute(expr)), parent.frame())
  err_sub <- eval(substitute(substitute(errmsg)), parent.frame())
  #   expr_sub <- eval(call("function", pairlist(x=NULL, values=NULL), expr_sub))  
  #   function(x, values) if (expr_sub(x, values)) NULL else err(x, values)
  function(x, vals) {
    res <- tryCatch(eval(expr_sub, list(x=x, vals=vals)), error=error)
    if (isTRUE(res)) return(TRUE) else 
      return(eval(err_sub, list(x=x, vals=vals)))
  }
}


all.checks <- function(...) {
  fns <- list(...)
  function(x, vals) {
    for (fn in fns) {
      res <- fn(x, vals)
      if (! isTRUE(res)) return(res)
    }
    return(TRUE)
  }
}


# we can have some default checkers
check_length <- function(minl=0, maxl=Inf, err) {
  if (missing(err)) err <- substitute(paste(x, "must be between", minl, "and", maxl, "characters long"))
  checker(! is.null(x) && nchar(x) %in% minl:maxl, err)
}
  
  