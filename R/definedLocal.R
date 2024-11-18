
#' @title Functions in a Package, defined in a \link[base]{local}
#' 
#' @param pkg \link[base]{character} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [definedLocal] returns an \link[base]{invisible} \link[base]{numeric} \link[base]{vector}.
#' 
#' @examples
#' # ?devtools::check error, but works for end-user
#' \dontrun{definedLocal('spatstat.explore')}
#' \dontrun{(definedLocal('spatstat.explore'))}
#' @export
definedLocal <- function(pkg, ...) {
  
  ns <- getNamespace(name = pkg)
  # as.environment(sprintf(fmt = 'package:%s', pkg)) # need to load into search()
  
  # https://stackoverflow.com/questions/12114355/show-names-of-everything-in-a-package
  x <- ls(name = ns, all.names = TRUE)
  
  obj <- mget(x, envir = ns)
  fn <- obj[vapply(obj, FUN = is.function, FUN.VALUE = NA)]
  ev <- lapply(fn, FUN = environment)
  id <- !vapply(ev, FUN = identical, y = ns, FUN.VALUE = NA)
  if (!any(id)) {
    message(sprintf(fmt = 'None of %d functions are defined in a ?base::local, etc.', length(id)))
  } else message(sprintf(fmt = '%d/%d (%.1f%%) functions are defined in a ?base::local, etc.', sum(id), length(id), 1e2*mean(id)))
  
  return(invisible(names(which(id))))
  
}