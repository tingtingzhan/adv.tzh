


#' @title Install Missing Packages
#' 
#' @description
#' ..
#' 
#' @param x \link[base]{character} scalar or \link[base]{vector}, package name(s)
#' 
#' @keywords internal
#' @importFrom utils installed.packages install.packages
#' @export
install.missing.packages <- function(x) {
  
  if (!length(x)) return(invisible())
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) stop('need character `x`')
  
  x1 <- installed.packages() |> # 'matrix'
    rownames() |>
    setdiff(x = x |> unique.default() |> sort(), y = _)
  
  if (!length(x1)) return(invisible())
  
  x1 |> 
    install.packages()
  
  #cat('Packages', paste(sQuote(x1), collapse = ', '), 'not installed.  Install from \n')
  #cat('Bioconductor \u2605 BiocManager::install()\n')
  #cat('Github \u2605 remotes::install_github()\n')
}






