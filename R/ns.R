

#' @title Attach \link[base:getNamespace]{Namespace} and `:` to Function Name
#' 
#' @param x a \link[utils]{getAnywhere} object
#' 
#' @returns
#' Function [ns_name()] returns a \link[base]{character} scalar or \link[base]{vector}.
#' 
#' @examples
#' 'rlnorm' |> getAnywhere() |> ns_name() # exported
#' 'format_perc' |> getAnywhere() |> ns_name() # not exported
#' library(dplyr); 'filter' |> getAnywhere() |> ns_name() # conflict
#' @keywords internal
#' @export
ns_name <- function(x) {
  if (!inherits(x, what = 'getAnywhere')) stop('input must be getAnywhere-class')
  pkg <- (x$where) |> 
    grep(pattern = '^namespace\\:', value = TRUE) |>
    gsub(pattern = '^namespace\\:', replacement = '')
  colon <- pkg |> 
    lapply(FUN = function(nm) { # (nm = pkg[[1L]])
      nm |> getNamespace() |> getNamespaceExports()
    }) |> 
    vapply(FUN = `%in%`, x = x$name, FUN.VALUE = NA) |>
    ifelse(yes = '::', no = ':::')
  paste0(pkg, colon, x$name)
}

if (FALSE) {
  # `x = 'format_perc'` errs, because stats:::format_perc is not exported
  x |> get() |> environment() |> getNamespaceName()
}






#' @title Attach \link[base:getNamespace]{Namespace} and `:` to *Imported* Function Name
#' 
#' @param fun \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param pkg \link[base]{character} scalar
#' 
#' @examples
#' # ns_fun_fromImports(fun = c('col_green', 'build_manual'), pkg = 'adv.tzh')
#' # fine with devtools::check() on \pkg{adv.tzh}
#' # but not okay if copied into other packages :)
#' @keywords internal
#' @export
ns_fun_fromImports <- function(fun, pkg) {
  impt0 <- getNamespaceImports(ns = getNamespace(name = pkg))
  impt <- impt0[nzchar(names(impt0))]
  x <- unlist(impt, use.names = FALSE)
  id <- match(fun, table = x, nomatch = NA_integer_)
  if (anyNA(id)) stop('stop, for now')
  paste0(names(impt)[id], '::', x[id])
}




if (FALSE) {
  
  # [format_perc] is unexported from \pkg{stats}
  
  ns = getNamespace('stats')
  stopifnot(identical(ns, asNamespace('stats')))
  
  stopifnot(exists('format_perc', envir = ns))
  stopifnot('format_perc' %in% ls(envir = ns))
  
  # ?`::` # read very carefully! - not helpful, though..
  # ?getFromNamespace
  
  getFromNamespace(x = 'format_perc', ns = ns)
  # fixInNamespace(x = 'format_perc', ns = ns) # um, disastrous
  
  tryCatch(getExportedValue(ns = ns, name = 'format_perc'), error = identity) # YES!!!!
  stopifnot(!'format_perc' %in% getNamespaceExports(ns = ns)) # YESSSS!!!!
  
}

