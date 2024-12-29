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
#' Function [dataFromPackage] returns a \link[base]{list} of R objects.
#' 
#' @references 
#' \url{https://stackoverflow.com/questions/27709936/get-a-list-of-the-data-sets-in-a-particular-package}
#' 
#' @examples
#' # dataFromPackage(c('datasets', 'MASS'), what = 'data.frame') # next step
#' 
#' lapply(dataFromPackage('datasets', what = 'data.frame'), head)
#' lapply(dataFromPackage('datasets', what = 'numeric'), head)
#' 
#' @importFrom utils data
#' @export
dataFromPackage <- function(pkg = 'datasets', what) {
  
  #message('\rGetting data from package ', pkg, '                       ', appendLF = FALSE)
  
  #env <- as.environment(paste0('package:', pkg)) # only allow len-1 `pkg`
  
  ev <- data(package = pkg) |> # allow vector `pkg`
    as.environment.packageIQR()
  
  if (!length(ev)) {
    message('no data sets found') # mimic ?utils:::print.packageIQR
    return(invisible())
  }
  
  ret <- if (is.list(ev)) {
    tmp <- lapply(ev, FUN = as.list.environment, sorted = TRUE)
    nms <- lapply(tmp, FUN = names)
    stop('have not figured out how to deal with duplicated names yet')
  } else as.list.environment(ev, sorted = TRUE)
  
  if (missing(what)) return(ret)
  
  id <- vapply(ret, FUN = inherits, what = what, FUN.VALUE = NA)
  return(ret[id])
}












#' @title S3 Method Dispatches for `'packageIQR'` Object
#' 
#' @param x `'packageIQR'` object, returned value of function \link[utils]{data},
#' all available data sets in one or more packages
#' 
#' @param ... addtional parameters, currently not in use
#' 
#' @details
#' Function [as.environment.packageIQR] re-runs function \link[utils]{data} 
#' in a \link[base]{new.env}.
#' 
#' @returns
#' Function [as.environment.packageIQR] returns an \link[base]{environment}.
#' 
#' @note
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
#' \link[base]{mget} is ~200 times faster than \link[utils]{data}.
#' 
#' Sometimes function \link[base]{mget} fails and I do not know why.
#' 
#' Function \link[utils]{data} looks for 'data' in *many* places.
#' 
#' @examples
#' ls1 = data(package = 'datasets') |> as.environment() |> as.list(sort = TRUE)
#' ls2 = as.environment('package:datasets') |> as.list(sort = TRUE)
#' stopifnot(identical(ls1, ls2))
#' 
#' if (FALSE) {
#' ls1 = data(package = 'spatstat.data') |> as.environment() |> as.list(sort = TRUE)
#' library(spatstat.data); ls2 = as.environment('package:spatstat.data') |> as.list(sort = TRUE)
#' length(ls1)
#' length(ls2)
#' setdiff(names(ls2), names(ls1))
#' class(ls2$copyExampleFiles) # function
#' }
#' 
#' \dontrun{
#' pkg = setdiff(rownames(installed.packages()), c('rjags', 'VennDiagram'))
#' ev = data(package = pkg) |> as.environment() # not that slow
#' length(ev)
#' }
#' @name packageIQR_S3
#' @importFrom utils data
#' @export as.environment.packageIQR
#' @export
as.environment.packageIQR <- function(x) {
  
  xs <- split.packageIQR(x)
  if (!length(xs)) return(invisible())

  # same data name could appear in more than one package(s)
  ev_ <- mapply(FUN = function(x, package) {
    ev <- new.env()
    nm <- sort.default(unique.default(gsub(pattern = '^.* \\(|\\)$', replacement = '', x = x$results[,'Item'])))
    data(list = nm, package = package, envir = ev) # must have `list` to assign object(s) to `envir`
    if (length(ls(envir = ev)) != dim(x$results)[1L]) stop('do not allow')
    return(ev)
  }, x = xs, package = names(xs), SIMPLIFY = FALSE)
  
  if (length(xs) == 1L) return(ev_[[1L]])
  
  return(ev_)
  
}


#' @rdname packageIQR_S3
#' 
#' @details
#' 
#' Function [split.packageIQR] is inspired by function `?utils:::print.packageIQR`.
#' 
#' @examples
#' x = data(package = c('datasets', 'survival'))
#' x1 = data(package = c('datasets'))
#' x2 = data(package = c('survival'))
#' y = split(x)
#' stopifnot(identical(y$datasets, x1), identical(y$survival, x2))
#' @export split.packageIQR
#' @export
split.packageIQR <- function(x, ...) {
  
  f <- 'Package' # hard-coded for now
  
  db <- x$results
  if (!length(db)) return(invisible()) # 'no data sets found'
  
  ids <- split.default(seq_len(nrow(db)), f = db[,'Package'])
  dbs <- lapply(ids, function(id) db[id, , drop = FALSE])
  
  ret <- lapply(ids, FUN = function(i) x)
  mapply(FUN = function(x, db) {
    x$results <- db
    return(x)
  }, x = ret, db = dbs, SIMPLIFY = FALSE)
  
}










#dataFromPackage_mget <- function(pkg = 'datasets') {
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
#  mget(nm, envir = as.environment(pkg_)) # still fail on some packages...
#}


  
  

