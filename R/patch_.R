
#' @title Create a Patch
#' 
#' @description
#' Create a *patch* to the input version.
#' 
#' @param v \link[base]{character} scalar, or \link[base]{numeric_version} object, 
#' package version
#' 
#' @details
#' A *patch* to the input version is created. For example,
#' \itemize{
#' \item `'2.3.1'` is returned if the input version is `'2.3'` or `'2.3.0'`
#' 
#' \item `'2.3.2'` is returned if the input version is `'2.3.1'`
#' }
#' 
#' @returns
#' Function [patch_()] returns a \link[base]{numeric_version} object.
#' 
#' @examples
#' patch_('7.3.6')
#' 'MASS' |> packageVersion() |> patch_()
#' 'ordinal' |> packageVersion() |> patch_()
#' @references 
#' \url{https://r-pkgs.org/release.html}
#' @export
patch_ <- function(v) {
  
  v <- v |> package_version()
  if (length(v) != 1L) stop('no need to vectorize')
  # print.default(v) # to understand 'numeric_version' object
  
  v_ <- unclass(v)[[1L]]
  nv <- length(v_)
  if (nv == 1L) stop('I dont think such package exists on CRAN')
  if (nv == 2L) { # could happen
    v_[3L] <- 1L # create a `patch` at 3rd place
  } else { # i.e., (nv >= 3L)
    v_[nv] <- v_[nv] + 1L # lowest tier plus 1
  }
  
  structure(list(v_), class = c('package_version', 'numeric_version'))
  
}

