
#' @title Working Package Dependencies
#' 
#' @description
#' Similar to \link[tools]{package_dependencies}, but for 
#' working packages (i.e., directories on hard drive to be loaded by function \link[pkgload]{load_all}).
#' 
#' @param path \link[base]{character} scalar
#' 
#' @param recursive \link[base]{logical} scalar, see \link[tools]{package_dependencies}
#' 
#' @param vanilla.rm \link[base]{logical} scalar, whether to remove packages in \link[base]{search} path
#' of a vanilla R session.  Default `TRUE`
#' 
#' @param ... additional parameters of \link[tools]{package_dependencies}
#' 
#' @note
#' I do not know how to make function \link[tools]{package_dependencies} work with packages on Github!
#' 
#' @examples
#' if (FALSE) {
#' working_package_dependencies('.', recursive = FALSE)
#' working_package_dependencies('.', recursive = TRUE)
#' working_package_dependencies('.', recursive = TRUE, vanilla.rm = FALSE)
#' working_package_dependencies_date('.')
#' 
#' working_package_dependencies('../DanielBiostatistics10th', recursive = FALSE)
#' working_package_dependencies('../DanielBiostatistics10th', recursive = TRUE)
#' working_package_dependencies('../DanielBiostatistics10th', recursive = TRUE, vanilla.rm = FALSE)
#' working_package_dependencies_date('../DanielBiostatistics10th')
#' }
#' @importFrom devtools as.package
#' @importFrom pkgload parse_deps
#' @importFrom tools package_dependencies
#' @name working_package_dependencies
#' @export
working_package_dependencies <- function(
    path = '.', 
    recursive = TRUE, 
    vanilla.rm = TRUE,
    ...
) {

  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) stop('illegal package `path`')
  
  pkg <- path |> 
    as.package()
  
  tmp <- pkg[c('depends', 'imports')] |>
    unlist(use.names = FALSE)
  # extracts Depends and Imports fields from DESCRIPTION file
  if (!length(tmp)) return(invisible())
  
  ret <- tmp |>
    lapply(FUN = function(j) parse_deps(j)$name) |>
    unlist(use.names = FALSE)
  # no matter packages in Depends and Imports are from CRAN or Github
  if (!recursive) return(ret)
  
  ret2 <- ret |>
    package_dependencies(recursive = TRUE, ...) |>
    # ?tools::package_dependencies does *not* work with packages from Github!!! 
    # read more about parameter `db` of ?tools::package_dependencies
    unlist(use.names = FALSE) |>
    unique.default() |>
    sort.int()
  
  if (vanilla.rm) {
    ret2 <- ret2 |> vanilla_search_rm()
  }
  
  return(ret2)

}


vanilla_search_rm <- function(x) {
  # `x` is a character vector of package names
  setdiff(x = x, y = c(
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
  dt <- pkg |>
    lapply(FUN = packageDate, ...) |>
    do.call(what = c) 
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
#' @param ... parameters of functions [working_package_dependencies] and \link[tools]{package_dependencies}
#' 
#' @examples
#' \dontrun{# needs to set CRAN mirror when ?devtools::check
#' package_dependencies_date(c('MASS', 'survival', 'nlme'), recursive = TRUE)}
#' @importFrom tools package_dependencies
#' @name package_dependencies_date
#' @export
package_dependencies_date <- function(...) {
  package_dependencies(...) |>
    lapply(FUN = vanilla_search_rm) |>
    lapply(FUN = sort_packageDate_)
}


#' @rdname package_dependencies_date
#' @export
working_package_dependencies_date <- function(...) {
  working_package_dependencies(...) |>
    sort_packageDate_()
}
