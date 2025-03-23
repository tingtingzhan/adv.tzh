
#' @title Clean Augment Files in Parent Directory
#' 
#' @param pkg ..
#' 
#' @examples
#' # clean_parent_dir(pkg = '../groupedHyperframe')
#' @importFrom pkgload pkg_name pkg_version
#' @export
clean_parent_dir <- function(pkg = '.') {
  
  path <- pkg |> normalizePath() # package path
  name <- path |> pkg_name()
  version <- path |> pkg_version()
  parent_dir <- path |> dirname()
  
  parent_dir |> 
    file.path(paste0(name, '_', version, '.pdf')) |>
    file.remove()
  
  parent_dir |> 
    file.path(paste0(name, '.Rcheck')) |>
    file.remove()
  
}