


#' @title Save an \link[base]{environment} to `.rda` File
#' 
#' @description
#' ..
#' 
#' @param envir an R object convertible to \link[base]{environment}
#' 
#' @param compress \link[base]{character} scalar, see \link[base]{save}.
#' Default `'xz'`
#' 
#' @param ... additional parameters of \link[base]{save}, other than
#' `list`, `envir` and `compress`
#' 
#' @export
save.envir <- function(
    envir = parent.frame(),
    compress = 'xz', 
    ...
) {
  envir <- as.environment(envir)
  save(list = ls(envir = envir), envir = envir, compress = compress, ...)
}