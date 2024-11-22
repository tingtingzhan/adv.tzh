
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
#' local_function('base')
#' local_function('stats')
#' local_function('utils')
#' local_function('tools')
#' local_function('MASS')
#' local_function('nlme')
#' local_function('survival')
#' @export
local_function <- function(pkg, ...) {
  
  ns <- getNamespace(name = pkg)
  # as.environment(sprintf(fmt = 'package:%s', pkg)) # need to load into search()
  
  # https://stackoverflow.com/questions/12114355/show-names-of-everything-in-a-package
  x <- ls(name = ns, all.names = TRUE, sorted = TRUE)
  
  obj <- mget(x, envir = ns)
  fn0 <- obj[vapply(obj, FUN = is.function, FUN.VALUE = NA)]
  
  # primitive functions (e.g., ?base::`-`) does not have an environment
  # environment(fun = base::`-`) # NULL
  fn <- fn0[!vapply(fn0, FUN = is.primitive, FUN.VALUE = NA)]
  
  ev <- lapply(fn, FUN = environment)
  
  # e.g., ?stats:::update.packageStatus; <environment: namespace:utils>
  # but \pkg{stats} does *not* import \pkg{utils} 
  #package_dependencies(pkg) # no use..
  
  # functions imported from another package/namespace, e.g., 
  # (env = environment(stats:::update.packageStatus))
  # isNamespace(env) # TRUE
  # isNamespace(environment(spatstat.explore::Emark)) # FALSE
  id <- !vapply(ev, FUN = isNamespace, FUN.VALUE = NA) & !vapply(ev, FUN = identical, y = ns, FUN.VALUE = NA)
  if (!any(id)) {
    message(sprintf(fmt = 'None of %d functions in package \033[1;35m%s\033[0m are defined via ?base::local, etc.', length(id), pkg))
    return(invisible())
  } 
  
  lapply(names(which(id)), FUN = function(f) { # (f = '-')
    env <- environment(fun = get(f, envir = ns))
    x <- setdiff(ls(envir = env, all.names = TRUE), y = f)
    message(sprintf(
      fmt = 'Local envir of \033[1;34m%s\033[0m contains %s', f,
      if (length(x)) {
        paste0('\033[1;33m', x, '\033[0m', collapse = ', ')
      } else '\033[1;32mnothing else\033[0m'
    ))
  })
  message()
  message(sprintf(fmt = '\033[1;32m%d/%d; %.1f%%\033[0m functions in package \033[1;36m%s\033[0m are defined via ?base::local, etc.', sum(id), length(id), 1e2*mean(id), pkg))

  return(invisible())
  
}


if (FALSE) {
  # run in vanilla R, not RStudio!
  x = as.data.frame.matrix(installed.packages())
  subset(x, !is.na(Priority) & Priority == 'base', select = 'Package')
  subset(x, !is.na(Priority) & Priority == 'recommended', select = 'Package')
}

