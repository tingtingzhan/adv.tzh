
#' @title Working Package Dependencies
#' 
#' @description
#' Similar to \link[tools]{package_dependencies}, but for 
#' working packages (i.e., directories on hard drive to be loaded by function \link[pkgload]{load_all}).
#' 
#' @param path \link[base]{character} scalar
#' 
#' @param recursive \link[base]{logical} scalar, see function \link[tools]{package_dependencies}
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
#' working_package_dependencies('.') |> sort_packageDate_()
#' }
#' @keywords internal
#' @importFrom devtools as.package
#' @importFrom pkgload parse_deps
#' @importFrom tools package_dependencies
#' @importFrom utils available.packages installed.packages
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
    lapply(FUN = \(j) parse_deps(j)$name) |>
    unlist(use.names = FALSE)
  # no matter packages in Depends and Imports are from CRAN or Github
  if (!recursive) return(ret)
  
  db <- available.packages() # see inside ?tools::package_dependencies
  
  ret1 <- ret |>
    setdiff(y = installed.packages(priority = 'base') |> rownames()) # these packages are not on CRAN
  
  id <- match(x = ret1, table = rownames(db), nomatch = NA_integer_)
  if (anyNA(id)) message('Package(s) ', paste(col_blue(ret1[is.na(id)]), collapse = ', '), ' not available on CRAN')
  # ?tools::package_dependencies does *not* work with packages from Github!!! 
  # read more about parameter `db` of ?tools::package_dependencies
  
  ret2 <- ret1[!is.na(id)] |>
    package_dependencies(recursive = TRUE, ...) |>
    unlist(use.names = FALSE) |>
    unique.default() |>
    sort.int()
  
  if (!vanilla.rm) return(ret2)
    
  return(setdiff(ret2, y = vanilla_search_()))

}


vanilla_search_ <- function() c(
  # packages loaded into search() path in vanilla R
  'base', 
  'datasets', 
  'graphics', 'grDevices', 
  'methods', 
  'stats',
  'utils' 
)

# https://stackoverflow.com/questions/9700799/difference-between-r-base-and-r-recommended-packages










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
#' Function [sort_packageDate_()] returns a \link[base]{Date} scalar or \link[base]{vector}.
#' 
#' @examples
#' # To pass CRAN check
#' 'https://cran.rstudio.com/' |>
#'  utils::contrib.url() |> 
#'  utils::available.packages() |> 
#'  tools::package_dependencies(c('grid', 'pkgload'), db = _, recursive = TRUE) |>
#'  lapply(FUN = sort_packageDate_)
#' 
#' \dontrun{# In RStudio, simply use
#' tools::package_dependencies(c('grid', 'pkgload'), recursive = TRUE) |>
#'  lapply(FUN = sort_packageDate_)
#' }
#' @keywords internal
#' @importFrom utils packageDate
#' @export
sort_packageDate_ <- function(
    pkg, decreasing = TRUE, 
    ...
) {
  if (!length(pkg)) return(invisible())
  pkg <- pkg |>
    setdiff(y = installed.packages(priority = 'base') |> rownames()) # will just be date of R 
  names(pkg) <- pkg
  pkg |>
    lapply(FUN = packageDate, ...) |>
    do.call(what = c) |> # ?base::sapply or ?base::vapply cannot keep Date class!
    sort.default(decreasing = decreasing)
}



