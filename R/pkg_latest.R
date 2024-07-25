


#' @title Latest Updated Packages
#' 
#' @description ..
#' 
#' @param within_days \link[base]{integer} scalar, 
#' number of days to look back (default `5L`)
#' 
#' @details 
#' Function [pkg_latest] returns the date of update for the most recently updated packages.
#' 
#' @examples 
#' pkg_latest()
#' 
#' @importFrom utils installed.packages packageDate
#' @export
pkg_latest <- function(within_days = 5L) {
  if (!is.numeric(within_days) || length(within_days) != 1L || anyNA(within_days)) stop('within_days must be len-1 numeric')
  pkg <- dimnames(installed.packages())[[1L]]
  names(pkg) <- pkg
  dts <- do.call(c, args = lapply(pkg, FUN = packageDate))
  out <- (dts[(Sys.Date() - dts) < within_days])
  message(sprintf('Packages updated in the last %d days:', within_days))
  print(sort.int(out, decreasing = TRUE))
}










# All *default* S3 method in here works with 
# \link[devtools:as.package]{package} and
# \link[utils]{packageDescription} objects.


# for package NEWS, use ?utils::news (slow)


