

#' @title Format Byte Size
#' 
#' @description 
#' Format byte size.
#' 
#' @param x \link[base]{numeric} scalar or \link[base]{vector}, byte size 
#' 
#' @param units \link[base]{character} scalar, 
#' see parameter `units` of function \link[utils]{format.object_size}
#' 
#' @details
#' Function [byte_format()] formats byte size
#' in the same manner as 
#' function \link[utils]{format.object_size} does to \link[utils]{object.size}.
#' 
#' @returns 
#' Function [byte_format()] returns a \link[base]{character} scalar or \link[base]{vector}.
#' 
#' @examples 
#' # './R' |> list.files(full.names = TRUE) |> file.size() |> byte_format(units = 'auto')
#' @keywords internal
#' @importFrom utils getS3method
#' @export
byte_format <- function(x, units = 'auto') {
  x |>
    vapply(FUN = getS3method(f = 'format', class = 'object_size'), units = units, FUN.VALUE = '')
}


