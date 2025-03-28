

#' @title Subset of S3/S4 Methods
#' 
#' @description ..
#' 
#' @param x a `'MethodsFunction'` object, returned value from functions 
#' \link[utils]{methods} and \link[utils]{.S3methods}
#' 
#' @param i subset criteria, see examples
#' 
#' @details ..
#' 
#' @note 
#' I am still playing with \link[utils]{getS3method} and \link[methods]{getMethod}
#' 
#' Not sure what is \link[methods]{getClass}
#' 
#' @returns 
#' Function \link{[.MethodsFunction} returns a `'MethodsFunction'` object.
#' 
#' @examples 
#' library(Matrix)
#' (x1 = .S3methods(head))
#' x1[1:3]
#' 
#' (x2 = methods(head))
#' x2[!isS4]
#' x2[isS4]
#' 
#' # developer's playground
#' utils::getS3method(f = 'head', class = 'data.frame') |> class() # 'function'
#' methods::getMethod(f = 'head', signature = 'Matrix') |> class() # 'MethodDefinition'
#' @name sub_MethodsFunction
#' @export
`[.MethodsFunction` <- function(x, i) {
  info <- attr(x, which = 'info', exact = TRUE) # 'data.frame', as of 2024-11-01; packageDate('utils')
  id <- eval(expr = substitute(i), envir = info)
  ret <- unclass(x)[id]
  attr(ret, which = 'info') <- info[id, , drop = FALSE]
  attr(ret, which = 'class') <- attr(x, which = 'class', exact = TRUE)
  attr(ret, which = 'byclass') <- attr(x, which = 'byclass', exact = TRUE)
  return(ret)
}


