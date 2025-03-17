
#' @title nextCRANversion
#' 
#' @description ..
#' 
#' @param name \link[base]{character} scalar, name of package
#' 
#' @param Version (optional) \link[base]{character} scalar, package version
#' 
#' @param repos \link[base]{character} scalar, see \link[utils]{contrib.url}.
#' Default `'https://cran.rstudio.com/'`, 
#' which is the return of `getOption('repos')` when using RStudio
#' 
#' @details 
#' 
#' If package \pkg{name} is not available on CRAN, i.e., this is the first release,
#' then `'0.1.0'` is returned.
#' 
#' Otherwise, if `Version` is missing, 
#' a patch to the current CRAN version is created, via [patch_].
#' 
#' Otherwise, if user-provided `Version` is older than CRAN version, 
#' then the patched CRAN version, as described previously, is returned.
#' 
#' Otherwise, the user-provided `Version` is returned.
#' 
#' @returns 
#' Function [nextCRANversion()] returns a \link[base]{numeric_version} object.
#' 
#' @examples
#' nextCRANversion('MASS')
#' nextCRANversion('MASS', Version = '3.0')
#' nextCRANversion('MASS', Version = '8.0')
#' 
#' @importFrom utils available.packages
#' @export
nextCRANversion <- function(name, Version, repos = 'https://cran.rstudio.com/') {
  
  # inside ?usethis::use_release_issue
  # see ?usethis:::choose_version -> ?usethis:::bump_version
  # real workhorse ?usethis:::bump_
  
  CRAN <- available.packages(filters = 'CRAN', repos = repos)
  
  if (!(name %in% dimnames(CRAN)[[1L]])) return('0.1.0') # my first release
  
  # ?base::package_version
  vCRAN <- package_version(CRAN[name, 'Version']) # version on CRAN; class 'package_version' inheriting from 'numeric_version'
  
  if (missing(Version) || !nzchar(Version)) return(patch_(vCRAN))
  vUser <- Version |> package_version()
  if (length(vUser) != 1L) stop('only one user version needed')
  if (vCRAN >= vUser) return(patch_(vCRAN))
  return(vUser)
  
}
