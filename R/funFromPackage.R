


#' @title Functions From a Package
#' 
#' @description
#' Obtain all \link[base]{function}s defined (*not* imported) in a package
#' 
#' @param pkg \link[base]{character} scalar, package name
#' 
#' @param all.names \link[base]{logical} scalar, see \link[base]{ls}.
#' Default `TRUE` to get functions with name starting with `'.'`
#' 
#' @param ... additional parameters of \link[base]{ls}
#' 
#' @details
#' Function [funFromPackage] ..
#' 
#' Function [funFromThisPackage] ..
#' 
#' @returns 
#' 
#' Functions [funFromPackage] and [funFromThisPackage] 
#' both return a \link[base]{character} \link[base]{vector} of function names.
#' 
#' @references
#' \url{https://stackoverflow.com/questions/8696158/find-all-functions-including-private-in-a-package}
#' 
#' @examples 
#' funFromPackage('stats', pattern = '\\.test$')
#' 
#' @name funFromPackage
#' @export
funFromPackage <- function(
    pkg, 
    all.names = TRUE, 
    ...
) {
  ls(envir = getNamespace(name = pkg), all.names = all.names, ...)
}


#' @rdname funFromPackage
#' @importFrom pkgload pkg_name
#' @export
funFromThisPackage <- function(all.names = TRUE, ...) {
  ls(envir = getNamespace(name = pkg_name('.')), all.names = all.names, ...)
}





