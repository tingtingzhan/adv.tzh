
#' @title Functions in a Package, defined inside \link[base]{local}
#' 
#' @param pkg \link[base]{character} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [local_function] does not have a returned value.
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
#' @name local_function
#' @importFrom cli style_bold col_green col_yellow col_blue col_magenta col_cyan
#' @export
local_function <- function(pkg, ...) {
  
  ns <- getNamespace(name = pkg)
  # as.environment(sprintf(fmt = 'package:%s', pkg)) # need to load into search()
  
  # https://stackoverflow.com/questions/12114355/show-names-of-everything-in-a-package
  x <- ls(name = ns, all.names = TRUE, sorted = TRUE)
  fn <- x[vapply(mget(x, envir = ns), FUN = is.function, FUN.VALUE = NA)]
  
  ret <- lapply(fn, FUN = function(i) { # (i = fn[[1L]])
    call(name = 'print_local_obj', call(name = ':::', as.name(pkg), as.name(i))) |> 
      eval()
  })
  
  id <- (lengths(ret, use.names = FALSE) > 0L)
  
  if (!any(id)) {
    sprintf(fmt = 'None of %d functions in package %s are defined via ?base::local, etc.', length(id), pkg |> col_magenta() |> style_bold()) |> 
      message()
    return(invisible())
  } 
  
  message(paste(unlist(ret[id]), collapse = '\n'))
  message()
  sprintf(
    fmt = '%s functions in package %s are defined via ?base::local, etc.', 
    sprintf(fmt = '%d/%d; %.1f%%', sum(id), length(id), 1e2*mean(id)) |> col_green() |> style_bold(), 
    pkg |> col_cyan() |> style_bold()
  ) |>
    message()
  
  return(invisible())
  
}



#' @rdname local_function
#' 
#' @param fun \link[base]{function}, must be given in the format of `pkg::function`
#' 
#' @details
#' Helper function [.local_obj()] provides the names of other objects 
#' in the \link[base]{local} environment of function `fun` definition.
#' 
#' @returns
#' Helper function [.local_obj] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' .local_obj(fun = base::sum) # primitive
#' .local_obj(fun = base::sub) # not defined in local environment
#' .local_obj(fun = stats:::update.packageStatus) # actually ?utils:::update.packageStatus
#' .local_obj(fun = base::.doSortWrap)
#' @export
.local_obj <- function(fun) {
  
  fun.name <- substitute(fun)
  if (!(as.character(fun.name[[1L]]) %in% c('::', ':::'))) stop('`fun` must be pkg::fun format')
  
  # primitive functions (e.g., ?base::`-`) does not have an environment
  # environment(fun = base::`-`) # NULL
  if (is.primitive(fun)) return(invisible())
  
  env <- environment(fun = fun)
  
  if (is.null(env)) stop('other than primitive function?')
  
  if (isNamespace(env)) return(invisible()) # ?base::getNamespace of this packge, or some other packages
  
  ls(envir = env, all.names = TRUE) |>
    setdiff(y = as.character(fun.name[[3L]]))
  
}

#' @rdname local_function
#' 
#' @details
#' Helper function [print_local_obj] prints the returned value of function [.local_obj]
#' in a pretty fashion (with the help of package \CRANpkg{cli}).
#' 
#' @returns 
#' Helper function [print_local_obj] returns an ANSI string, 
#' see \link[cli]{ansi-styles}.
#' 
#' @examples
#' message(print_local_obj(fun = base::.doSortWrap))
#' @export
print_local_obj <- function(fun) {
  
  x <- call(name = '.local_obj', fun = substitute(fun)) |> 
    eval()
  if (!length(x)) return(invisible())
  
  return(sprintf(
    fmt = 'Local envir of %s contains %s', 
    (substitute(fun)[[3L]]) |> as.character() |> col_blue() |> style_bold(),
    if (length(x)) {
      x |> col_yellow() |> style_bold() |> paste0(collapse = ', ')
    } else 'nothing else' |> col_green() |> style_bold()
  ))
  
} 


#' @rdname local_function
#' 
#' @param envir an \link[base]{environment} to load the \link[base]{local} objects.
#' Default `.GlobalEnv`
#' 
#' @details
#' Helper function [load_local_obj] loads the returned value of function [.local_obj]
#' into a user-specified \link[base]{environment}.
#' 
#' @returns 
#' Helper function [load_local_obj] does not have a returned value
#' 
#' @examples
#' load_local_obj(fun = base::.doSortWrap)
#' @export
load_local_obj <- function(fun, envir = .GlobalEnv) {
  
  x <- call(name = '.local_obj', fun = substitute(fun)) |> 
    eval()
  if (!length(x)) return(invisible())
  
  from <- environment(fun = fun)
  lapply(x, FUN = function(i) { # (i = x[[1L]])
    assign(x = i, value = get(x = i, envir = from), envir = envir)
  })
  
  return(invisible())
  
}













if (FALSE) {
  # run in vanilla R, not RStudio!
  x = as.data.frame.matrix(installed.packages())
  subset(x, !is.na(Priority) & Priority == 'base', select = 'Package')
  subset(x, !is.na(Priority) & Priority == 'recommended', select = 'Package')
}

