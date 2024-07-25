#' @title Data From Package
#' 
#' @description
#' Obtain all objects available via \link[utils]{data} from a package
#' 
#' @param pkg \link[base]{character} scalar or \link[base]{vector}, default `'datasets'`
#' 
#' @param what \link[base]{character} scalar or \link[base]{vector}, class(es), see \link[base]{inherits}
#' 
#' @returns 
#' Function [dataFromPackage] returns a \link[base]{list} of R objects
#' 
#' @references 
#' \url{https://stackoverflow.com/questions/27709936/get-a-list-of-the-data-sets-in-a-particular-package}
#' 
#' @examples
#' lapply(dataFromPackage('datasets', what = 'data.frame'), head)
#' lapply(dataFromPackage('datasets', what = 'numeric'), head)
#' # lapply(dataFromPackage(c('MASS', 'survival')), head)
#' 
#' # for developers
#' data(); ls()
#' tryCatch(data(beaver1, package = 'datasets'), warning = identity); ls()
#' data(beavers, package = 'datasets'); ls()
#' @importFrom utils data
#' @export
dataFromPackage <- function(pkg = 'datasets', what) {
  # ?utils::data looks for 'data' in *many* places
  env <- packageIQR2env(data(package = pkg))
  
  ret <- as.list.environment(env, sorted = TRUE)
  if (missing(what)) return(ret)
    
  id <- vapply(ret, FUN = inherits, what = what, FUN.VALUE = NA)
  return(ret[id])
}


packageIQR2env <- function(object) {
  
  # `object` is a 'packageIQR' object, returned from ?utils::data 
  
  res <- object[['results']]
  item <- res[, 'Item']
  pkgs <- unique.default(res[, 'Package'])
  
  load_nm <- sort.default(unique.default(gsub(pattern = '^.* \\(|\\)$', replacement = '', x = item)))
  # data name without parentheses, e.g. `ChickWeight`, 
  # .. can be loaded by `data(ChickWeight, package = 'datasets')`
  # data name with parentheses, e.g. `fdeaths (UKLungDeaths)`, 
  # .. can be loaded by `data(UKLungDeaths, package = 'datasets')`,
  # .. canNOT be loaded by `data(fdeaths, package = 'datasets')`
  # .. these are objects saved to UKLungDeaths.rda file in the source!!
  
  env <- new.env()
  data(list = load_nm, package = pkgs, envir = env)
  if (length(ls(envir = env)) != dim(res)[1L]) stop('did not obtain all data ?')
  return(env)

}

  
  

