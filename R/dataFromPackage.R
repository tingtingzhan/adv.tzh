#' @title Data From Package
#' 
#' @description
#' Obtain all objects available via \link[utils]{data} from a package
#' 
#' @param pkg \link[base]{character} scalar or \link[base]{vector}, default `'datasets'`
#' 
#' @param what \link[base]{character} scalar or \link[base]{vector}, class(es), see \link[base]{inherits}
#' 
#' @details
#' For data name without parentheses, e.g. `CO2`, 
#' \itemize{
#' \item {works: `data(list = 'CO2', package = 'datasets')`}
#' \item {works: `mget('CO2', envir = as.environment('package:datasets'))`}
#' }
#' 
#' Data names with parentheses, e.g. `beaver1 (beavers)`,
#' are objects saved to `beavers.rda` file in the source.
#' \itemize{
#' \item {works: `data(list = 'beavers', package = 'datasets')`}
#' \item {no! `data(list = 'beaver1', package = 'datasets')`}
#' \item {no! `mget(x = 'beavers', envir = as.environment('package:datasets'))`}
#' \item {works: `mget(x = 'beaver1', envir = as.environment('package:datasets'))`}
#' }
#' 
#' \link[base]{mget} is ~200 times faster than \link[utils]{data}, 
#' 
#' Sometimes \link[base]{mget} fails and I do not know why.
#' 
#' \link[utils]{data} looks for 'data' in *many* places.
#' 
#' @returns 
#' Function [dataFromPackage] returns a \link[base]{list} of R objects.
#' 
#' @references 
#' \url{https://stackoverflow.com/questions/27709936/get-a-list-of-the-data-sets-in-a-particular-package}
#' 
#' @examples
#' lapply(dataFromPackage('datasets', what = 'data.frame'), head)
#' lapply(dataFromPackage('datasets', what = 'numeric'), head)
#' 
#' \dontrun{
#' pkg = setdiff(rownames(installed.packages()), c('rjags', 'VennDiagram'))
#' noout = lapply(pkg, FUN = dataFromPackage)
#' }
#' @importFrom utils data
#' @export
dataFromPackage <- function(pkg = 'datasets', what) {
  
  # message('\r', pkg, '                       ', appendLF = FALSE)
  
  env <- packageIQR2env(data(package = pkg))
  
  ret <- as.list.environment(env, sorted = TRUE)
  if (missing(what)) return(ret)
  
  id <- vapply(ret, FUN = inherits, what = what, FUN.VALUE = NA)
  return(ret[id])
}


packageIQR2env <- function(object) {
  
  # `object` is a 'packageIQR' object, returned from ?utils::data 
  
  res <- object[['results']]
  
  nm <- sort.default(unique.default(gsub(pattern = '^.* \\(|\\)$', replacement = '', x = res[,'Item'])))
  
  env <- new.env()
  data(list = nm, package = unique.default(res[,'Package']), envir = env)
  if (length(ls(envir = env)) != dim(res)[1L]) stop('did not obtain all data ?')
  return(env)
  
}






#dataFromPackage_mget <- function(pkg = 'datasets', what) {
#  message('\r', pkg, appendLF = FALSE)
#  if (length(pkg) != 1L) stop('`pkg` must be length-1')
#  IQR_ <- data(package = pkg) # ?utils::data looks for 'data' in *many* places
#  res <- IQR_[['results']]
# nm <- vapply(strsplit(res[,'Item'], split = ' '), FUN = `[[`, 1L, FUN.VALUE = '')
#  pkg_ <- paste0('package:', pkg)
#  if (!(pkg_ %in% search())) {
#    attach(getNamespace(pkg), name = pkg_)
#    on.exit(detach(name = pkg_, unload = TRUE, character.only = TRUE))
#  }
#  ret <- mget(nm, envir = as.environment(pkg_)) # still fail on some packages...
#  if (missing(what)) return(ret)
#  id <- vapply(ret, FUN = inherits, what = what, FUN.VALUE = NA)
#  return(ret[id])
#}


  
  

