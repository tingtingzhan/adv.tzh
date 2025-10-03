


#' @title Functions From a Package
#' 
#' @description
#' Obtain all \link[base]{function}s defined (*not* imported) in a package
#' 
#' @param x see **Usage**
#' 
#' @param what parameter of function \link[base]{inherits}
#' 
#' @param all.names,sorted,... additional parameters of function \link[base]{ls}
#' 
#' @details
#' Function [objectFrom()] ..
#' 
#' @returns 
#' 
#' Functions [objectFrom()] returns a \link[base]{character} \link[base]{vector} of function names.
#' 
#' @references
#' \url{https://stackoverflow.com/questions/8696158/find-all-functions-including-private-in-a-package}
#' 
#' @examples 
#' objectFrom('stats', pattern = '\\.test$')
#' objectFrom('stats', what = 'selfStart')
#' # objectFrom('.') # for developer only
#' 
#' @name objectFrom
#' @export
objectFrom <- function(x, ...) UseMethod(generic = 'objectFrom')

#' @rdname objectFrom
#' @export objectFrom.environment
#' @export
objectFrom.environment <- function(
    x, 
    what, 
    all.names = TRUE, 
    sorted = TRUE, 
    ...
) {
  
  v <- ls(envir = x, all.names = all.names, sorted = sorted, ...)
  if (missing(what)) return(v)
  
  id <- v |>
    mget(x = _, envir = x) |>
    vapply(FUN = inherits, what = what, FUN.VALUE = NA)
  return(v[id])
  
}

#' @rdname objectFrom
#' @importFrom pkgload pkg_name
#' @export objectFrom.character
#' @export
objectFrom.character <- function(x, ...) {
  if (length(x) != 1L) stop('must be scalar')
  if (dir.exists(x)) x <- pkg_name(path = x)
  x |> 
    getNamespace() |> # returns 'environment'
    objectFrom.environment(...)
}




