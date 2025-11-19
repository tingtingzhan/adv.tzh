


#' @title [vennS3]
#' 
#' @param pkg \link[base]{character} scalar
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
#' vennS3('survival')
#' vennS3('caret')
#' vennS3('ggplot2')
#' vennS3('plotly')
#' 
#' # vennS3('spatstat.geom') # base::local() definition causes error..
#' }
#' 
#' 
#' @keywords internal
#' @importFrom methods isGroup getGroupMembers
#' @importFrom parallel mclapply
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
        match(table = names(base_obj)) # all NA !!
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
  
  list(
    Primitive = z_primitive,
    'S3 Generic' = z_generic,
    'S3 Method' = z_method,
    'Member of Group Generic' = z_groupMember
  ) |>
    venn(all. = (pkg != 'base'), print.mode = 'raw')
    
}