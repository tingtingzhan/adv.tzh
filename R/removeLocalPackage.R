

#' @title Remove Old Version of a Package
#' 
#' @description Remove old version of a package from \link[base]{search} path, 
#' as well as from \link[utils]{installed.packages}.
#' 
#' @param name \link[base]{character} scalar, name of the package
#'  
#' @importFrom pkgload pkg_name
#' @importFrom utils installed.packages remove.packages
#' @export
removeLocalPackage <- function(name = '.') {
  if (identical(name, '.')) name <- pkg_name('.')
  search_name <- paste0('package:', name)
  if (search_name %in% search()) detach(name = search_name, unload = TRUE, character.only = TRUE)
  for (lib in .libPaths()) {
    if (name %in% rownames(installed.packages(lib.loc = lib))) remove.packages(pkgs = name, lib = lib)
  }
  unlink(file.path(lib, paste0('00LOCK-', name)), recursive = TRUE)
}


