
#' @title Update DESCRIPTION File
#' 
#' @description Update DESCRIPTION file
#' 
#' @param pkg \link[base]{character} scalar, directory of the package
#' 
#' @details
#' Update the DESCRIPTION file, so that
#' 
#' \describe{
#' 
#' \item{`Version`}{is the next version on CRAN, via [nextCRANversion]}
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
  
  #pkg <- normalizePath(pkg)
  #name <- basename(pkg)
  name <- pkg |> normalizePath() |> basename()
  
  use_version(which = which)
  
  if (!file.exists(DESC_file <- file.path(pkg, 'DESCRIPTION'))) stop('missing DESCRIPTION file?')
  dcf <- read.dcf(file = DESC_file) # 'matrix'
  nm <- dimnames(dcf)[[2]]
  
  #if (any(nm == 'Version')) {
  #  dcf[, 'Version'] <- format.numeric_version(nextCRANversion(name = name, Version = dcf[, 'Version']))
  #} else dcf <- cbind(dcf, Version = format.numeric_version(nextCRANversion(name = name)))
  # ?base::format.numeric_version inside ?base::as.character.numeric_version
  
  if (any(nm == 'Date')) {
    dcf[, 'Date'] <- as.character.Date(Sys.Date())
  } else dcf <- cbind(dcf, Date = as.character.Date(Sys.Date()))
  
  #Author <- paste(format(eval(str2lang(dcf[, 'Authors@R'])), include = c('given', 'family', 'role', 'comment')), collapse = ',\n')
  # ?utils:::as.character.person, workhorse ?utils:::format.person
  # CRAN finally takes care of this, since Spring 2024 !!!
  # I have published to CRAN without 'Author' field
  
  write.dcf(dcf, file = DESC_file) # it's not read-only

}

