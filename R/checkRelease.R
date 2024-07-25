
#' @title Additional Checks Before \link[devtools]{release}
#' 
#' @description additional checks before \link[devtools]{release}
#' 
#' @param pkg \link[base]{character} scalar, directory of the package
#' 
#' @param to_release \link[base]{logical} scalar
#' 
#' @importFrom devtools check check_rhub check_win_devel release 
#' @export
checkRelease <- function(pkg, to_release = TRUE) {
  
  pkg <- normalizePath(pkg)

  # setwd(dir = pkg); usethis::use_cran_comments() # ?base::setwd does not work; file created in Rproj `tzh` (instead of Rproj *name*)
  
  check(pkg = pkg, document = FALSE, cran = TRUE, remote = FALSE, manual = TRUE) # see ?devtools::release
  #return(invisible())
  
  #noout <- build(pkg = pkg, binary = FALSE)
  # @importFrom devtools build
  #install(pkg = pkg)
  
  if (!to_release) {
    # ?devtools::check_rhub (workhorse ?rhub::check_for_cran) too slow ..
    c_rhub <- tryCatch(check_rhub(pkg = pkg), error = identity)
    if (inherits(c_rhub, what = 'error')) { 
      if (c_rhub[['message']] == 'Email address not validated') {
        stop('run rhub::validate_email(email = \'tingtingzhan@gmail.com\')')
      } else stop('unknown error message?')
    }
    check_win_devel(pkg = pkg)
  } else release(pkg = pkg, check = FALSE)
}
