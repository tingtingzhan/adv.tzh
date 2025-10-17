
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
#' local_function('graphics')
#' local_function('utils')
#' local_function('tools')
#' local_function('MASS')
#' # local_function('spatstat.geom')
#' # local_function('spatstat.explore')
#' @keywords internal
#' @name local_function
#' @export
local_function <- function(pkg, ...) {
  
  ns <- getNamespace(name = pkg)

  # https://stackoverflow.com/questions/12114355/show-names-of-everything-in-a-package
  x <- ls(name = ns, all.names = TRUE, sorted = TRUE)
  is_fn <- x |>
    mget(envir = ns) |>
    vapply(FUN = is.function, FUN.VALUE = NA)
  ret <- x[is_fn] |> 
    lapply(FUN = \(i) { # (i = x[is_fn][[1L]])
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
  
  ev <- environment(fun = fun)
  
  if (is.null(ev)) stop('other than primitive function?')
  
  if (isNamespace(ev)) return(invisible()) # ?base::getNamespace of this packge, or some other packages
  
  ret <- ls(envir = ev, all.names = TRUE)
  attr(ret, which = 'main') <- (fun.name[[3L]]) |> 
    as.character()
  attr(ret, which = 'envir') <- ev
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
  
  .main <- x |> 
    attr(which = 'main', exact = TRUE)
  
  z0 <- x0 <- unclass(x)
  main_id <- (x0 == .main)
  dots_id <- (x0 == '...')
  if (any(main_id)) {
    z0[main_id] <- x0[main_id] |> 
      col_yellow()
  }
  z0[!main_id] <- x0[!main_id] |>
    col_br_magenta()
  z0[dots_id] <- x0[dots_id] |>
    col_br_red() |> style_bold() |> bg_br_yellow()
  
  sprintf(
    fmt = 'Local envir of %s contains %s', 
    .main |> col_blue() |> style_bold(),
    if (!all(main_id)) {
      z0 |> paste0(collapse = ', ')
    } else {
      'nothing else' |> col_green() |> style_bold()
    }
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


