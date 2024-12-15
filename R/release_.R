
# old name [checkRelease]

#' @title Auxiliary Tasks before \link[devtools]{release}
#' 
#' @description 
#' Auxiliary tasks before \link[devtools]{release}, including
#' \itemize{
#' \item {\link[devtools]{check}}
#' \item {`rhub::rhub_check`; still working on this}
#' \item {\link[devtools]{check_win_devel}}
#' }
#' 
#' @param pkg \link[base]{character} scalar, directory of the package
#' 
# @param to_release \link[base]{logical} scalar
#' 
#' @importFrom devtools check check_win_devel release 
#' @export
release_ <- function(pkg) { # , to_release = TRUE
  
  pkg <- normalizePath(pkg)

  # setwd(dir = pkg); usethis::use_cran_comments() # ?base::setwd does not work; file created in Rproj `tzh` (instead of Rproj *name*)
  
  check(pkg = pkg, document = FALSE, cran = TRUE, remote = FALSE, manual = TRUE) # see ?devtools::release
  #return(invisible())
  
  #noout <- build(pkg = pkg, binary = FALSE)
  # @importFrom devtools build
  #install(pkg = pkg)
  
  # deprecated/defunct from ?rhub::check_for_cran (inside ?devtools::check_rhub). See
  # https://github.com/r-lib/devtools/issues/2570
  # https://github.com/r-lib/devtools/issues/2500; Hadley points to ?usethis::use_release_issue(), ouch!!! 
  if (FALSE) {
  #  rhub. <- tryCatch(check_rhub(pkg = pkg), error = identity)
  #  if (inherits(rhub., what = 'error')) { 
  #    if (rhub.[['message']] == 'Email address not validated') {
  #      stop('run rhub::validate_email(email = \'tingtingzhan@gmail.com\')')
  #    } else stop('unknown error message?')
  #  }
  } # old version
  
  # read ?rhubv2 .. this is tough!!
  # @importFrom rhub rhub_check
  # rhub_check(gh_url = NULL, platforms = NULL, r_versions = NULL, branch = NULL) # looks like to be the solution
  
  check_win_devel(pkg = pkg)
  
  release(pkg = pkg, check = FALSE)
  
}
