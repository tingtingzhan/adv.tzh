
#' @title Create a Patch to Current CRAN Version
#' 
#' @description
#' Create a *patch* to the current CRAN Version.
#' 
#' @param v \link[base]{character} scalar, or \link[base]{numeric_version} object, 
#' package version
#' 
#' @details
#' A *patch* to the current CRAN version is created. For example,
#' \itemize{
#' \item `'2.3.1'` is returned if the current CRAN version is `'2.3'` or `'2.3.0'`
#' 
#' \item `'2.3.2'` is returned if the current CRAN version is `'2.3.1'`
#' }
#' 
#' @returns
#' [patchVersion] returns a \link[base]{numeric_version} object.
#' 
#' @examples
#' patchVersion('7.3.6')
#' patchVersion(packageVersion('MASS'))
#' 
#' @references 
#' \url{https://r-pkgs.org/release.html}
#' 
#' @export
patchVersion <- function(v) {
  
  # see my notes inside [nextCRANversion]
  
  if (is.character(v) && (length(v) == 1L) && !is.na(v) && nzchar(v)) {
    v <- package_version(v)
  }
  if (!inherits(v, what = 'package_version')) stop('input cannot be converted to \'package_version\'')
  
  # print.default(v) # to understand 'numeric_version' object
  
  v_ <- unclass(v)[[1L]]
  nv <- length(v_)
  if (nv == 1L) stop('I dont think such package exists on CRAN')
  v_[3L] <- if (nv == 2L) { # could happen
    1L # create a `patch`
  } else {
    v_[3L] + 1L # the next `patch`
    # some packages could have (nv == 4L)
  }
  
  structure(list(v_), class = c('package_version', 'numeric_version'))
}

