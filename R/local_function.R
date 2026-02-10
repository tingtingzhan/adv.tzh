
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
#' \dontrun{
#' local_function('spatstat.explore')
#' local_function('spatstat.geom')
#' local_function('spatstat.random')
#' }
#' @keywords internal
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
        eval() |>
        suppressMessages()
    })

  id <- (lengths(ret, use.names = FALSE) > 0L)
  
  if (!any(id)) {
    sprintf(
      fmt = 'None of %d functions in package %s %s are defined via {.fun base::local}, etc.', 
      length(id), 
      pkg |> col_magenta() |> style_bold(),
      pkg |> packageVersion() |> as.character() |> sprintf(fmt = 'ver%s') |> col_br_blue() |> style_bold()
    ) |> 
      cli_text()
      message(appendLF = FALSE)
    return(invisible())
  } 
  
  ret[id] |> 
    lapply(FUN = print.local_obj)
  message()
  sprintf(
    fmt = '%s functions in package %s %s are defined via {.fun base::local}, etc.', 
    sprintf(fmt = '%d/%d (%.1f%%)', sum(id), length(id), 1e2*mean(id)) |> col_green() |> style_bold(), 
    pkg |> col_cyan() |> style_bold(),
    pkg |> packageVersion() |> as.character() |> sprintf(fmt = 'ver%s') |> col_br_blue() |> style_bold()
  ) |>
    cli_text() |>
    message(appendLF = FALSE)
  
  return(invisible())
  
}



#' @title Objects in a Local Environment
#' 
#' @param fun \link[base]{function}, must be given in the format of `pkg::function`
#' 
#' @details
#' Function [.local_obj()] provides the names of other objects 
#' in the \link[base]{local} environment of `fun`ction definition.
#' 
#' @returns
#' Function [.local_obj()] returns an object of class `'local_obj'`.
#' 
#' @examples
#' .local_obj(fun = base::sum)
#' .local_obj(fun = base::sub)
#' .local_obj(fun = stats:::update.packageStatus) # actually ?utils:::update.packageStatus
#' .local_obj(fun = base::.doSortWrap)
#' @keywords internal
#' @importFrom stats setNames
#' @export
.local_obj <- function(fun) {
  
  fun.name <- substitute(fun)
  if (!(as.character(fun.name[[1L]]) %in% c('::', ':::'))) stop('`fun` must be pkg::fun format')
  
  # primitive functions (e.g., ?base::`-`) does not have an environment
  # environment(fun = base::`-`) # NULL
  if (is.primitive(fun)) { 
    fun.name |>
      deparse1() |>
      col_magenta() |> style_bold() |>
      sprintf(fmt = '%s is a .Primitive function') |>
      message()
    return(invisible())
  }
  
  ev <- environment(fun = fun)
  
  if (is.null(ev)) stop('non-primitive function must have non-NULL environment')
  
  if (isNamespace(ev)) {
    # ?base::getNamespace of this packge, or some other packages
    fun.name |>
      deparse1() |>
      col_yellow() |> style_bold() |>
      sprintf(fmt = '%s is not defined in a local environment') |>
      message()
    return(invisible()) 
  }
  
  ret <- ls(envir = ev, all.names = TRUE) |>
    setNames(nm = _) |>
    lapply(FUN = get, envir = ev)
  # `ret` is a named list of objects
  attr(ret, which = 'main') <- (fun.name[[3L]]) |> 
    as.character()
  class(ret) <- c('local_obj', class(ret)) |>
    unique.default()
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
#' Function [print.local_obj()] prints an ANSI string, 
#' see \link[cli]{ansi-styles}, 
#' but does not have a returned value.
#' 
#' @examples
#' .local_obj(fun = base::.doSortWrap) |> 
#'   print(details = TRUE)
#' @keywords internal
#' @export print.local_obj
#' @export
print.local_obj <- function(x, details = FALSE, ...) {
  
  .main <- x |> 
    attr(which = 'main', exact = TRUE)
  
  z0 <- x0 <- names(x)
  id_main <- (x0 == .main)
  id_dots <- (x0 == '...')

  tp <- x |>
    vapply(FUN = typeof, FUN.VALUE = NA_character_)
  id_closure_nonmain <- (tp == 'closure') & !id_main # function
  id_constant <- (tp != 'closure')
  
  z0[id_main] <- x0[id_main] |> 
    sprintf(fmt = '%s()') |>
    col_yellow()
  z0[id_dots] <- x0[id_dots] |>
    col_br_red() |> style_bold() |> bg_br_yellow()
  z0[id_closure_nonmain] <- x0[id_closure_nonmain] |>
    sprintf(fmt = '%s()') |>
    col_br_magenta() |> style_bold()
  z0[id_constant] <- x0[id_constant] |>
    make_ansi_style('grey70')()
  
  sprintf(
    fmt = 'Local envir of %s contains %s', 
    .main |> 
      #as.symbol() |>
      #deparse1() |>
      #sprintf(fmt = '%s()') |>
      col_blue() |> style_bold(),
    if (!all(id_main)) {
      z0 |> paste0(collapse = ', ')
    } else {
      'nothing else' |> col_green() |> style_bold()
    }
  ) |>
    message()
  
  if (details) {
    x |>
      print.default()
  }
  
  return(invisible())
  
} 


