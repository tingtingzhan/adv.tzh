



#' @title Check if Packages are Installed
#' 
#' @description
#' ..
#' 
#' @param x \link[base]{character} scalar or \link[base]{vector}, package name(s)
#' 
#' @importFrom utils installed.packages
#' @export
pkg_installed <- function(x) {
  if (!length(x)) return(invisible())
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) stop('need character name of packages')
  
  # packages installed from Github not in `rownames(installed.packages())`, why?
  inst <- dimnames(installed.packages())[[1L]]
  
  x <- setdiff(unique.default(x), c('R', '.', inst))
  if (!length(x)) return(invisible())
  
  cat('Packages', paste(sQuote(x), collapse = ', '), 'not installed.  Install from \n')
  cat('CRAN \u2605 utils::install.packages()\n')
  cat('Bioconductor \u2605 BiocManager::install()\n')
  cat('Github \u2605 devtools::install_github()\n')
}






