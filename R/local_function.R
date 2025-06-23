
#' @title Functions in a Package, defined inside \link[base]{local}
#' 
#' @param pkg \link[base]{character} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [local_function()] does not have a returned value.
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
#' @keywords internal
#' @name local_function
#' @importFrom cli cli_text style_bold col_green col_yellow col_blue col_magenta col_cyan
#' @export
local_function <- function(pkg, ...) {
  
  ns <- getNamespace(name = pkg)

  # https://stackoverflow.com/questions/12114355/show-names-of-everything-in-a-package
  x <- ls(name = ns, all.names = TRUE, sorted = TRUE)
  fn <- x[vapply(mget(x, envir = ns), FUN = is.function, FUN.VALUE = NA)]
  
  ret <- fn |> 
    lapply(FUN = \(i) { # (i = fn[[1L]])
      call(name = '.local_obj', call(name = ':::', as.symbol(pkg), as.symbol(i))) |> 
        eval()
    })

  id <- (lengths(ret, use.names = FALSE) > 0L)
  
  if (!any(id)) {
    sprintf(
      fmt = 'None of %d functions in package %s are defined via {.fun base::local}, etc.', 
      length(id), 
      pkg |> col_magenta() |> style_bold()
    ) |> 
      cli_text()
      message(appendLF = FALSE)
    return(invisible())
  } 
  
  ret[id] |> lapply(FUN = print.local_obj)
  message()
  sprintf(
    fmt = '%s functions in package %s are defined via {.fun base::local}, etc.', 
    sprintf(fmt = '%d/%d; %.1f%%', sum(id), length(id), 1e2*mean(id)) |> col_green() |> style_bold(), 
    pkg |> col_cyan() |> style_bold()
  ) |>
    cli_text() |>
    message(appendLF = FALSE)
  
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
#' Helper function [.local_obj()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' # Helper function
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
  
  main_function <- (fun.name[[3L]]) |> as.character()
  
  ret <- ls(envir = env, all.names = TRUE) |>
    setdiff(y = main_function)
  attr(ret, which = 'main_function') <- main_function
  attr(ret, which = 'envir') <- env
  class(ret) <- 'local_obj'
  return(ret)
}




#' @title print.local_obj
#' 
#' @param x returned object from function [.local_obj()]
#' 
#' @param details \link[base]{logical} scalar, whether to print all local objects.
#' Default `FALSE`.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @returns 
#' Function [print.local_obj()] prints in an ANSI string, 
#' see \link[cli]{ansi-styles}.
#' 
#' @returns 
#' Function [print.local_obj()] does not have a returned value.
#' 
#' @examples
#' .local_obj(fun = base::.doSortWrap) |> print(details = TRUE)
#' @keywords internal
#' @importFrom stats setNames
#' @export print.local_obj
#' @export
print.local_obj <- function(x, details = FALSE, ...) {
  
  sprintf(
    fmt = 'Local envir of %s contains %s', 
    x |> attr(which = 'main_function', exact = TRUE) |> col_blue() |> style_bold(),
    if (length(x)) {
      x |> col_yellow() |> style_bold() |> paste0(collapse = ', ')
    } else 'nothing else' |> col_green() |> style_bold()
  ) |>
    message()
  
  if (details) {
    x |>
      setNames(nm = x) |>
      lapply(FUN = get, envir = x |> attr(which = 'envir', exact = TRUE)) |>
      print()
  }
  
  return(invisible())
  
} 







if (FALSE) {
  # run in vanilla R, not RStudio!
  x = as.data.frame.matrix(installed.packages())
  subset(x, !is.na(Priority) & Priority == 'base', select = 'Package')
  subset(x, !is.na(Priority) & Priority == 'recommended', select = 'Package')
}

