
#' @title Functions in a Package, defined inside \link[base]{local}
#' 
#' @param pkg \link[base]{character} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [local_function] returns an \link[base]{invisible} \link[base]{numeric} \link[base]{vector}.
#' 
#' @note
#' Need to **Suggests** for examples to run.
#' 
#' @examples
#' local_function('base') # wow
#' local_function('stats') # wow
#' local_function('utils') # wow
#' local_function('tools') # wow
#' local_function('MASS')
#' local_function('nlme')
#' local_function('survival')
#' @export
local_function <- function(pkg, ...) {
  
  ns <- getNamespace(name = pkg)
  # as.environment(sprintf(fmt = 'package:%s', pkg)) # need to load into search()
  
  # https://stackoverflow.com/questions/12114355/show-names-of-everything-in-a-package
  x <- ls(name = ns, all.names = TRUE)
  
  obj <- mget(x, envir = ns)
  fn <- obj[vapply(obj, FUN = is.function, FUN.VALUE = NA)]
  ev <- lapply(fn, FUN = environment)
  id <- !vapply(ev, FUN = identical, y = ns, FUN.VALUE = NA)
  if (!any(id)) {
    message(sprintf(fmt = 'None of %d functions are defined via ?base::local, etc.', length(id)))
  } else message(sprintf(fmt = '%d/%d (%.1f%%) functions are defined via ?base::local, etc.', sum(id), length(id), 1e2*mean(id)))
  
  return(invisible(names(which(id))))
  
}


if (FALSE) {
  # run in vanilla R, not RStudio!
  x = as.data.frame.matrix(installed.packages())
  subset(x, !is.na(Priority) & Priority == 'base', select = 'Package')
  subset(x, !is.na(Priority) & Priority == 'recommended', select = 'Package')
}

