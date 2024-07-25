

# old name [pkg_dis]


#' @title Working Package Dependencies
#' 
#' @description
#' Similar to \link[tools]{package_dependencies}, but for 
#' working packages (i.e., directories on hard drive to be loaded by \link[pkgload]{load_all})
#' 
#' @param pkg \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param recursive \link[base]{logical} scalar, see \link[tools]{package_dependencies}
#' 
#' @param ... additional parameters of \link[tools]{package_dependencies}
#' 
#' @examples
#' if (FALSE) {
#' pkg = c('.', '../DanielBiostatistics10th',
#'   '../TukeyGH77')
#' working_package_dependencies(pkg, recursive = FALSE)
#' working_package_dependencies(pkg, recursive = TRUE)
#' working_package_dependencies_ext(pkg, recursive = TRUE)
#' working_package_dependencies_date(pkg)
#' }
#' 
#' 
#' @importFrom devtools as.package
#' @importFrom pkgload parse_deps pkg_name
#' @importFrom tools package_dependencies
#' @name working_package_dependencies
#' @export
working_package_dependencies <- function(pkg = '.', recursive = TRUE, ...) {

  if (!is.character(pkg) || anyNA(pkg) || !all(nzchar(pkg))) stop('only takes len-1 character as package name')
  
  names(pkg) <- vapply(pkg, FUN = pkg_name, FUN.VALUE = '')
  
  ret <- lapply(pkg, FUN = function(i) {
    tmp <- unlist(as.package(x = i)[c('depends', 'imports')], use.names = FALSE)
    if (!length(tmp)) return(invisible())
    unlist(lapply(tmp, FUN = function(j) parse_deps(j)$name), use.names = FALSE)
  })
  
  if (!recursive) return(ret)
  
  lapply(ret, FUN = function(i) {
    sort.int(unique.default(unlist(package_dependencies(i, recursive = TRUE, ...), use.names = FALSE)))
  })
  
}


#' @rdname working_package_dependencies
#' @export
working_package_dependencies_ext <- function(...) {
  lapply(working_package_dependencies(...), FUN = setdiff, y = c(
    'base', 'stats', 'graphics', 'grDevices', 'utils', 'datasets', 'methods' # packages loaded with vanilla R
  ))
}

#' @rdname working_package_dependencies
#' @importFrom tools package_dependencies
#' @export
package_dependencies_ext <- function(..., recursive = TRUE) {
  lapply(package_dependencies(..., recursive = recursive), FUN = setdiff, y = c(
    'base', 'stats', 'graphics', 'grDevices', 'utils', 'datasets', 'methods' # packages loaded with vanilla R
  ))
}



#' @title Last Update Dates of Packages, in Reverse Order
#' 
#' @description
#' ..
#' 
#' @param pkg \link[base]{character} scalar or \link[base]{vector},
#' packages on CRAN
#' 
#' @param decreasing \link[base]{logical} scalar, default `TRUE`.
#' See \link[base]{order}
#' 
#' @param ... additional parameters of \link[utils]{packageDate}
#' 
#' @returns
#' Function [sort_packageDate_] returns a \link[base]{Date} scalar or \link[base]{vector}.
#' 
#' @keywords internal
#' @importFrom utils packageDate
#' @export
sort_packageDate_ <- function(
    pkg, decreasing = TRUE, 
    ...
) {
  if (!length(pkg)) return(invisible())
  names(pkg) <- pkg
  dt <- do.call(c, args = lapply(pkg, FUN = packageDate, ...)) 
  # ?base::sapply or ?base::vapply cannot keep Date class!
  if (anyNA(dt)) stop('do not allow')
  dt[order(dt, decreasing = decreasing)] 
  # see ?base::sort.default; 'Date' object base::is.object
}



#' @title Last Update Dates of Packages Dependencies, in Reverse Order
#' 
#' @description
#' ..
#' 
#' @param ... parameters of [package_dependencies_ext] or [working_package_dependencies_ext]
#' 
#' @name package_dependencies_date
#' @export
package_dependencies_date <- function(...) {
  lapply(package_dependencies_ext(...), FUN = sort_packageDate_)
}

#' @rdname package_dependencies_date
#' @export
working_package_dependencies_date <- function(...) {
  lapply(working_package_dependencies_ext(...), FUN = sort_packageDate_)
}
