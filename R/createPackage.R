

#' @title Create Sub-Package
#' 
#' @description
#' Create sub-packages from my master package **tzh**
#' 
#' @param name \link[base]{character} scalar, name of the package
#' 
#' @param path \link[base]{character} scalar, parent directory of the package,
#' default is the return of \link[base]{tempdir}
#' 
#' @param to_release \link[base]{logical} scalar
#' 
#' @param ... additional parameters of [package_skeleton_ext]
#' 
#' @details 
#' 
#' This function can only be run by developer (i.e., me) from my master package **tzh**.
#' 
#' \link[Rcpp]{Rcpp.package.skeleton} is next step.
#' 
#' 
#' @references 
#' 
#' \url{https://cran.r-project.org/web/packages/policies.html}
#' 
#' My packages
#' \url{https://cran.r-project.org/package=QuantileGH}
#' 
#' \url{https://cran.r-project.org/package=DanielBiostatistics10th}
#' 
#' \url{https://cran.r-project.org/package=DemographicTable}
#' 
#' \url{https://cran.r-project.org/package=ThomasJeffersonUniv}
#' 
#' @export
createPackage <- function(name, path = tempdir(), to_release = TRUE, ...) {
  pkg <- file.path(path, name)
  package_skeleton_ext(name = name, path = path, ...)
  removeLocalPackage(name = name)
  updateDESCRIPTION(pkg = pkg)
  checkDocument(pkg = pkg)
  checkRelease(pkg = pkg, to_release = to_release)
  removeLocalPackage(name = name)
}
















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
#' a patch to the current CRAN version is created, via [patchVersion].
#' 
#' Otherwise, if user-provided `Version` is older than CRAN version, 
#' then the patched CRAN version, as described previously, is returned.
#' 
#' Otherwise, the user-provided `Version` is returned.
#' 
#' @returns 
#' Function [nextCRANversion] returns a \link[base]{numeric_version} object.
#' 
#' @examples
#' nextCRANversion('MASS')
#' nextCRANversion('MASS', Version = '3.0')
#' nextCRANversion('MASS', Version = '8.0')
#' 
#' @importFrom utils available.packages
#' @export
nextCRANversion <- function(name, Version, repos = 'https://cran.rstudio.com/') {

  CRAN <- available.packages(filters = 'CRAN', repos = repos)
  
  if (!(name %in% dimnames(CRAN)[[1L]])) return('0.1.0') # my first release
  
  vCRAN <- package_version(CRAN[name, 'Version']) # version on CRAN; class 'package_version' inheriting from 'numeric_version'
  
  if (missing(Version) || !nzchar(Version)) return(patchVersion(vCRAN))
  
  vUser <- if (is.character(Version) && (length(Version) == 1L) && !is.na(Version) && nzchar(Version)) {
    package_version(Version)
  } else Version
  if (!inherits(vUser, what = 'package_version')) stop('input cannot be converted to \'package_version\'')
  
  if (length(vUser) != 1L) stop('only one version needed')
  if (vCRAN >= vUser) return(patchVersion(vCRAN))
  return(vUser)

}


#' @title Create a Patch to Current CRAN Version
#' 
#' @description
#' Create a *patch* to the current CRAN Version.
#' 
#' @param v \link[base]{character} scalar, or \link[base]{numeric_version} object, 
#' package version
#' 
#' @details
#' A *patch* to the current CRAN version is created. For example,
#' \itemize{
#' \item `'2.3.1'` is returned if the current CRAN version is `'2.3'` or `'2.3.0'`
#' 
#' \item `'2.3.2'` is returned if the current CRAN version is `'2.3.1'`
#' }
#' 
#' @returns
#' [patchVersion] returns a \link[base]{numeric_version} object.
#' 
#' @examples
#' patchVersion('7.3.6')
#' patchVersion(packageVersion('MASS'))
#' 
#' @references 
#' \url{https://r-pkgs.org/release.html}
#' 
#' @export
patchVersion <- function(v) {
  if (is.character(v) && (length(v) == 1L) && !is.na(v) && nzchar(v)) {
    v <- package_version(v)
  }
  if (!inherits(v, what = 'package_version')) stop('input cannot be converted to \'package_version\'')
  
  # print.default(v) # to understand 'numeric_version' object
  
  v_ <- unclass(v)[[1L]]
  nv <- length(v_)
  if (nv == 1L) stop('I dont think such package exists on CRAN')
  v_[3L] <- if (nv == 2L) { # could happen
    1L # create a `patch`
  } else {
    v_[3L] + 1L # the next `patch`
    # some packages could have (nv == 4L)
  }
  
  structure(list(v_), class = c('package_version', 'numeric_version'))
}



