

#' @title `S3` methods table in vignette
#' 
#' @param generic.function,class see function \link[utils]{methods}
#' 
#' @param package \link[base]{character} scalar
#' 
#' @param package_pattern \link[base]{character} scalar of \link[base]{regex}
#' 
#' @param backtick \link[base]{logical} scalar, whether to put backticks around function names.
#' Default `TRUE` for Markdown/Quarto rendering.
#' 
#' @param ... additional parameters of the function \link[utils]{methods}
#' 
#' @returns 
#' The function [methods2kable()] returns a \link[base]{data.frame}.
#' 
#' @keywords internal
#' @importFrom utils methods packageVersion
#' @importFrom knitr kable
#' @export
methods2kable <- \(
  generic.function, class, # see function \link[utils]{methods}
  package, # \link[base]{character} scalar
  package_pattern, # \link[base]{character} scalar of \link[base]{regex}
  backtick = TRUE, # \link[base]{logical} scalar, whether to put backticks around function names. Default `TRUE` for Markdown/Quarto rendering.
  include_non_exported = TRUE, # \link[base]{logical} scalar, whether to include non-exported S3 methods (`'registered S3method'`)
  ... # additional parameters of the function \link[utils]{methods}
) {
  
  if (!missing(package)) {
    cl <- if (include_non_exported) {
      quote((from %in% package) | startsWith(from, prefix = 'registered S3method'))
    } else {
      quote(from %in% package)
    }
  } else if (!missing(package_pattern)) {
    cl <- quote(grepl(pattern = package_pattern, x = from))
  } else cl <- TRUE
  
  MFinfo <- \(...) {
    methods(...) |> 
      attr(which = 'info', exact = TRUE)
  }
  
  mf_info <- if (!missing(generic.function)) {
    if (length(generic.function) > 1L) {
      generic.function |>
        lapply(FUN = MFinfo, ...) |>
        do.call(what = rbind.data.frame, args = _)
    } else {
      MFinfo(generic.function = generic.function, ...)
    }
  } else if (!missing(class)) {
    MFinfo(class = class, ...)
  } else stop()
  
  x <- mf_info |>
    subset.data.frame(subset = eval(cl)) |>
    within.data.frame(expr = {
      
      if (
        (length(from) > 1L & all(duplicated.default(from)[-1L])) ||
        all(startsWith(from, prefix = 'registered S3method'))
      ) {
        from <- NULL
      }
      
      if (length(generic) > 1L & all(duplicated.default(generic)[-1L])) {
        generic <- NULL
      } else {
        generic <- generic |> 
          vapply(FUN = .ns_generic, backtick = backtick, ver = FALSE, FUN.VALUE = '')
      }
      
    })
  
  if (backtick) {
    rownames(x) <- x |>
      rownames() |> 
      sprintf(fmt = '`%s`')
  }
  
  s4 <- x$isS4 |>
    unique()
  oo <- if (all(s4)) '`S4`' else if (!any(s4)) '`S3`' else '`S3`/`S4`'
  
  if (!missing(generic.function)) {
    
    caption <- if (length(generic.function) > 1L) {
      
      if (!missing(package) && length(package) == 1L) {
        sprintf(fmt = '`%s::%s.*`', package, generic.function) |>
          paste(collapse = ', ') |>
          sprintf(fmt = '%s methods %s (v%s)', oo, . = _, packageVersion(package))
      } else { # missing(package) || length(package) > 1
        NULL # lazy way out :))
      }
      
    } else { # length(generic.function) == 1L
      generic.function |>
        .ns_generic(backtick = TRUE, ver = TRUE) |>
        sprintf(fmt = '%s methods of %s', oo, . = _)
    }
    
  } else if (!missing(class)) {
    
    caption <- if (!missing(package)) {
      sprintf(fmt = '%s methods `%s::*.%s` (v%s)', oo, package, class, packageVersion(package))
    } else {
      sprintf(fmt = '%s methods `*.%s` (%s)', oo, class, R.version.string)
    }
    
  } else stop()
    
  x |> 
    kable(caption = caption)
  
}



#' @title Generic Function with Namespace
#' 
#' @param x \link[base]{character} scalar
#' 
#' @param backtick \link[base]{logical} scalar
#' 
#' @param ver \link[base]{logical} scalar
#' 
#' @examples
#' .ns_generic('names<-')
#' .ns_generic('Math', ver = TRUE)
#' .ns_generic('update')
#' 
#' @keywords internal
#' @importFrom methods isGroup
#' @importFrom utils packageVersion
#' @export
.ns_generic <- \(
  x, # \link[base]{character} scalar
  backtick = TRUE, # \link[base]{logical} scalar
  ver = FALSE # \link[base]{logical} scalar
) {
  
  .sugar <- endsWith(x, suffix = '<-')
  
  # base::parse(text = _) cannot deal with syntactic sugar
  fn <- x |> 
    get()
  
  if (is.primitive(fn)) {
    ns <- 'base' 
  } else if (isGroup(x)) { # groupGeneric
    ns <- 'methods'
  } else if (
    inherits(fn, what = c('nonstandardGenericFunction', 'standardGeneric')) && 
    (attr(fn, which = 'package', exact = TRUE) == 'methods')
  ) {
    ns <- 'methods' # S4 generic function from \pkg{methods}
    # e.g., ?methods::initialize is nonstandardGenericFunction,
    # while ?methods::show is standardGeneric
  } else {
    ev <- fn |>
      environment()
    if (!isNamespace(ev)) stop(x, 'dont support yet..')
    ns <- ev |> 
      getNamespaceName()
  }
  
  z <- sprintf(fmt = if (.sugar) '%s::`%s`' else '%s::%s', ns, x)
  
  if (backtick) {
    z <- sprintf(fmt = if (.sugar) '`` %s ``' else '`%s`', z)
  }
  
  if (ver) {
    z <- sprintf(fmt = '%s (v%s)', z, packageVersion(ns))
  }
  
  return(z)
  
}






#' @title [rds2versiondate]
#' 
#' @param x \link[base]{matrix}, e.g., from \url{https://cran.r-project.org/web/packages/packages.rds}
#' 
#' @param pkg \link[base]{character} \link[base]{vector}
#' 
#' @param format \link[base]{Date} format, see functions \link[base]{format.Date} and \link[base]{strptime} for detail
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @export
rds2versiondate <- \(
  x, # \link[base]{matrix}, e.g., from \url{https://cran.r-project.org/web/packages/packages.rds}
  pkg, # \link[base]{character} \link[base]{vector}
  format = '%a %b %d, %Y', # \link[base]{Date} format, see functions \link[base]{format.Date} and \link[base]{strptime} for detail
  ...
) {
  x |>
    as.data.frame.matrix() |>
    subset.data.frame(subset = Package %in% pkg, select = c('Package', 'Version', 'Date')) |> # ?base::subset.data.frame
    within.data.frame(expr = {
      Package = Package |> 
        sprintf(fmt = '**`%s`**')
      Date = Date |>
        as.Date.character(format = '%Y-%m-%d') |>
        format.Date(format = format)
      Version = ifelse(
        test = is.na(Date),
        yes = Version,
        no = sprintf(fmt = '%s \U0001f5d3\ufe0f %s', Version, Date)
      )
      Date = NULL
    }) # ?base::
}
