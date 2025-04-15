


#' @title File Modification Time, Reverse Chronical
#' 
#' @description ..
#' 
#' @param path \link[base]{character} scalar, directory on hard drive, see \link[base]{list.files}
#' 
#' @param pattern \link[base]{character} scalar, regular expression \link[base]{regex}
#' 
#' @param file (optional) \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param ... ..
#' 
#' @examples 
#' \dontrun{ # devtools::check error
#' file_mtime('./R', pattern = '\\.R$')
#' }
#' 
#' @seealso 
#' \link[base]{file.info} \link[base]{file.mtime}
#' 
#' @keywords internal
#' @export
file_mtime <- function(
    path,
    pattern = '\\.xlsx$|\\.xls$|\\.csv$',
    file = list.files(path = path, pattern = pattern, ..., full.names = TRUE), # ?base::list.files do not allow `...`
    ...
) {
  
  if (!length(file)) stop('`path` not exists, or does not have `files` with given `pattern`')

  # ?tools::file_path_sans_ext can further remove file extension
  
  file |>
    file.info(extra_cols = FALSE) |> # see ?base::file.mtime and ?base::file.info
    subset.data.frame(select = c('mtime')) |>
    sort_by.data.frame(y = ~ list(- as.numeric(mtime)))
  
}


