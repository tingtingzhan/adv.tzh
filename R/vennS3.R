


#' @title Venn Graph of `S3` Generic & Methods
#' 
#' @param pkg \link[base]{character} scalar, name of package
#' 
#' @param mc.cores parameter of function \link[parallel]{mclapply}
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @examples
#' \dontrun{
#' vennS3('base')
#' vennS3('stats')
#' vennS3('graphics')
#' vennS3('utils')
#' vennS3('tools')
#' 
#' vennS3('MASS')
#' vennS3('nlme')
#' vennS3('mgcv')
#' vennS3('survival')
#' vennS3('caret')
#' vennS3('ggplot2')
#' vennS3('plotly')
#' vennS3('R6')
#' vennS3('S7')
#' 
#' vennS3('groupedHyperframe')
#' 
#' # vennS3('spatstat.geom') # base::local() definition causes error..
#' }
#' 
#' 
#' @keywords internal
#' @importFrom methods isGroup getGroupMembers
#' @importFrom parallel mclapply
#' @importFrom utils head
#' @importFrom sloop is_s3_generic is_s3_method
#' @importFrom grid.tzh venn
#' @export
vennS3 <- function(
    pkg,
    mc.cores = getOption('cores'),
    ...
) {
  
  ns <- pkg |> 
    getNamespace(name = _)
  
  obj <- ns |>
    as.list.environment()
  
  id_fun <- obj |>
    vapply(FUN = is.function, FUN.VALUE = NA)
  
  if (FALSE) {
    id_fun |>
      mean.default()
  }
  
  fun <- obj[id_fun]
  
  if (pkg == 'base') {
    
    z_primitive <- fun |>
      vapply(FUN = is.primitive, FUN.VALUE = NA)
    names(fun)[z_primitive] |>
      sort.int() |>
      head(n = 10L) |>
      col_magenta() |> style_bold() |>
      paste(collapse = ', ') |>
      sprintf(fmt = 'Primitive Functions: %s, etc.') |>
      message()
    
    methods_obj <- 'methods' |>
      getNamespace(name = _) |>
      as.list.environment()
    
    id_Group <- methods_obj |>
      names() |> # must!!
      vapply(FUN = isGroup, FUN.VALUE = NA)
    
    if (FALSE) {
      # ?base::groupGeneric
      # "(There are no objects of these names in base R, but there are in the methods package.)"
      methods_obj[id_Group] |> 
        names() |>
        match(table = names(obj)) # all NA !!
    }
    
    z_groupMember <- methods_obj[id_Group] |> 
      names() |>
      setdiff(y = c(
        'Ops' # ?methods::S4groupGeneric # "Note that Ops merely consists of three sub groups."
      )) |> 
      lapply(FUN = getGroupMembers) |>
      unlist(use.names = FALSE) |>
      match(x = names(fun), table = _, nomatch = NA_integer_) |>
      is.finite()
    
  } else {
    
    z_primitive <- z_groupMember <- NULL 
    
  }

  z_generic <- fun |>
    names() |>
    # vapply(FUN = is_s3_generic, env = ns, FUN.VALUE = NA) # too slow
    mclapply(mc.cores = mc.cores, FUN = is_s3_generic, env = ns) |>
    unlist()
  names(fun)[z_generic] |>
    sort.int() |>
    head(n = 10L) |>
    col_yellow() |> style_bold() |>
    paste(collapse = ', ') |>
    sprintf(fmt = if (sum(z_generic) > 10L) {
      'Generic Functions: %s, etc.'
    } else 'Generic Functions: %s') |>
    message()
  
  z_method <- fun |>
    names() |>
    # vapply(FUN = is_s3_method, env = ns, FUN.VALUE = NA)
    mclapply(mc.cores = mc.cores, FUN = is_s3_method, env = ns) |>
    unlist()
  
  if (FALSE) {
    if (pkg == 'base') {
      fun[z_groupMember & (!z_generic)] # `%*%`
      stopifnot(!is_s3_generic('%*%')) # really?
      methods(generic.function = '%*%') # nothing
    }
    if (pkg == 'survival') {
      fun[z_generic & z_method]
      # ?sloop::is_s3_generic # is pretty naive :))
      # well, the workhorse function ?sloop:::parse_method of ?sloop::is_s3_method is naive too..
    }
  }
  
  z_generic <- z_generic & (!z_method)
  
  z <- list(
    Primitive = z_primitive,
    'S3 Generic' = z_generic,
    'S3 Method' = z_method,
    'Group Generic' = z_groupMember
  ) |>
    venn(all. = (pkg != 'base'), all.nm = sprintf(fmt = 'pkg{%s}', pkg), print.mode = 'raw')
  
  attr(z, which = 'generic') <- names(fun)[z_generic]
  attr(z, which = 'Primitive') <- names(fun)[z_primitive]
  
  return(z)
    
}