#' @title Data From Package
#' 
#' @description
#' Obtain all objects available via \link[utils]{data} from a package
#' 
#' @param x a `packageIQR` object, returned value from \link[utils]{data}
#' 
#' @param what \link[base]{character} scalar or \link[base]{vector}, class(es), see \link[base]{inherits}
#' 

#' @returns 
#' Function [dataFrom()] returns a \link[base]{list} of R objects.
#' 
#' @references 
#' \url{https://stackoverflow.com/questions/27709936/get-a-list-of-the-data-sets-in-a-particular-package}
#' 
#' @examples
#' dataFrom(what = 'data.frame') |> lapply(FUN = head)
#' dataFrom(what = 'numeric') |> lapply(FUN = head)
#' data(package = c('datasets', 'MASS')) |> dataFrom(what = 'data.frame') |> lapply(FUN = head)
#' 
#' vcdExtra::datasets(package = 'datasets') # similar goal
#' @keywords internal
#' @importFrom utils data
#' @export
dataFrom <- function(x = data(package = 'datasets'), what) {
  
  if (!inherits(x, what = 'packageIQR')) stop('input must be the return of utils::data')
  
  ev <- x |> # allow vector `pkg`
    as.environment.packageIQR()
  
  if (!length(ev)) {
    message('no data sets found') # mimic ?utils:::print.packageIQR
    return(invisible())
  }
  
  foo <- \(envir, pkg) {
    ret <- as.list.environment(envir, sorted = TRUE)
    names(ret) <- paste0(pkg, '::', names(ret))
    return(ret)
  }
  
  ret <- if (is.environment(ev)) {
    foo(envir = ev, pkg = x$results[1L, 'Package'])
  } else if (is.list(ev)) {
    mapply(FUN = foo, envir = ev, pkg = names(ev), MoreArgs = NULL, SIMPLIFY = FALSE) |>
      unname() |>
      unlist(recursive = FALSE)
  }
  
  
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
#' Function [as.environment.packageIQR()] re-runs function \link[utils]{data} 
#' in a \link[base]{new.env}.
#' 
#' @returns
#' Function [as.environment.packageIQR()] returns an \link[base]{environment}.
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
#' \dontrun{
#' pkg = setdiff(rownames(installed.packages()), c('rjags', 'VennDiagram'))
#' # still trying to understand what's wrong with \CRANpkg{VennDiagram}
#' ev = data(package = pkg) |> as.environment() # not that slow
#' length(ev)
#' }
#' @keywords internal
#' @name packageIQR_S3
#' @importFrom utils data
#' @export as.environment.packageIQR
#' @export
as.environment.packageIQR <- function(x) {
  
  xs <- split.packageIQR(x)
  if (!length(xs)) return(invisible())

  # same data name could appear in more than one package(s)
  ev_ <- mapply(FUN = \(x, package) {
    ev <- new.env()
    nm <- gsub(pattern = '^.* \\(|\\)$', replacement = '', x = x$results[,'Item']) |> 
      unique.default() |> 
      sort.default()
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
#' Function [split.packageIQR()] is inspired by function `?utils:::print.packageIQR`.
#' 
#' @examples
#' x = data(package = c('datasets', 'MASS'))
#' x1 = data(package = c('datasets'))
#' x2 = data(package = c('MASS'))
#' y = split(x)
#' stopifnot(identical(y$datasets, x1), identical(y$MASS, x2))
#' @keywords internal
#' @export split.packageIQR
#' @export
split.packageIQR <- function(x, ...) {
  
  # x$results # 'matrix'
  if (!length(x$results)) return(invisible()) # 'no data sets found'
  
  dbs <- x$results |>
    nrow() |>
    seq_len() |>
    split.default(f = x$results[,'Package']) |>
    lapply(FUN = \(i) x$results[i, , drop = FALSE])
  
  ret <- dbs |>
    length() |>
    replicate(expr = x, simplify = FALSE)
  names(ret) <- names(dbs)
  
  mapply(FUN = \(x, db) {
    x$results <- db
    return(x)
  }, x = ret, db = dbs, SIMPLIFY = FALSE)

}



  
  

