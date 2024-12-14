

#' @title addNamespace
#' 
#' @param x a \link[utils]{getAnywhere} object
#' 
#' @returns
#' Function [addNamespace] returns a \link[base]{character} scalar or \link[base]{vector}.
#' 
#' @examples
#' addNamespace(getAnywhere('rlnorm'))
#' addNamespace(getAnywhere('format_perc'))
#' @keywords internal
#' @export
addNamespace <- function(x) {
  y <- grep(pattern = '^namespace\\:', x = x$where, value = TRUE)
  #if (length(y) != 1L) stop('now allows')
  pkgs <- gsub(pattern = '^namespace\\:', replacement = '', y)
  expts <- lapply(pkgs, FUN = function(pkg) { # (pkg = pkgs[[1L]])
    getNamespaceExports(ns = getNamespace(name = pkg))
  })
  paste0(
    pkgs, 
    ifelse(
      test = vapply(expts, FUN = `%in%`, x = x$name, FUN.VALUE = NA),
      yes = '::', 
      no = ':::'),
    x$name)
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

