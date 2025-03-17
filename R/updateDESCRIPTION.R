
#' @title Update DESCRIPTION File
#' 
#' @description Update DESCRIPTION file
#' 
#' @param pkg \link[base]{character} scalar, directory of the package
#' 
#' @param which see function \link[usethis]{use_version}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Update the DESCRIPTION file, so that
#' 
#' \describe{
#' 
#' \item{`Version`}{using \link[usethis]{use_version} with `which = 'dev'`}
#' 
#' \item{`Date`}{is today (via \link[base]{Sys.Date})} 
#' 
#' }
#' 
#' @importFrom usethis use_version
#' @export
updateDESCRIPTION <- function(
    pkg = '.',
    which = 'dev',
    ...
) {
  
  name <- pkg |> normalizePath() |> basename()
  
  if (!file.exists(DESC <- file.path(pkg, 'DESCRIPTION'))) stop('missing DESCRIPTION file?')
  dcf <- read.dcf(file = DESC) # 'matrix'
  nm <- dimnames(dcf)[[2]]
  
  if (any(nm == 'Date')) {
    dcf[, 'Date'] <- as.character.Date(Sys.Date())
  } else dcf <- cbind(dcf, Date = as.character.Date(Sys.Date()))
  
  # 'Author' field is determined by 'Authors@R' field
  
  write.dcf(dcf, file = DESC) # it's not read-only
  
  use_version(which = which)

}

