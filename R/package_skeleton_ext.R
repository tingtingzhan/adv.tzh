
#' @title Extend \link[utils]{package.skeleton}
#' 
#' @description
#' Extend \link[utils]{package.skeleton}
#' 
#' @param name,path,code_files see \link[utils]{package.skeleton}
#' 
#' @param Title \link[base]{character} scalar
#' 
#' @param Version (optional) \link[base]{character} scalar
#' 
#' @param Description \link[base]{character} scalar
#' 
#' @param AuthorsR a \link[utils]{person} \link[base]{call}.  
#' The `'Author'` and `'Maintainer'` fields in DESCRIPTION file will be generated **by CRAN** from this argument
#' 
#' @param Depends \link[base]{character} \link[base]{vector},
#' names of packages
#' 
#' @param Imports \link[base]{character} \link[base]{vector},
#' names of packages
#' 
#' @param Suggests \link[base]{character} \link[base]{vector},
#' names of packages
#' 
#' @param cpp_files next step, using `Rcpp::Rcpp.package.skeleton`
#' 
#' @param rda_files (optional) \link[base]{character} \link[base]{vector}
#' 
#' @param object2rda (optional) \link[base]{character} \link[base]{vector}, name(s) of objects to be
#' saved in an `.rda` file.
#' 
#' @param vignette_files \link[base]{character} \link[base]{vector}
#' 
#' @param ... ..
#' 
#' @details 
#' 
#' This function can only be run by developer (i.e., me) from my master package **tzh**.
#' 
#' @references 
#' \url{https://cran.r-project.org/web/packages/policies.html}
#' 
#' @importFrom utils package.skeleton person
#' @export
package_skeleton_ext <- function(
    name, path,
    code_files = stop(''), cpp_files = character(), 
    rda_files = character(), object2rda = character(),
    vignette_files = character(),
    Title = stop('package `Title` missing'), 
    Version = '0.1.0',
    Description = stop(''),
    AuthorsR = c(person(given = 'Tingting', family = 'Zhan', role = c('aut', 'cre', 'cph'), email = c('tingtingzhan@gmail.com', 'Tingting.Zhan@jefferson.edu'), comment = c(ORCID = '0000-0001-9971-4844'))),
    Depends = character(), Imports = character(), Suggests = character(),
    ...
) {
  
  pkg <- file.path(path, name)
  unlink(pkg, recursive = TRUE) # delete directory
  unlink(paste0(pkg, '.Rcheck'), recursive = TRUE) # delete directory
  file.remove(list.files(path = path, pattern = paste0('^', name, '_'), full.names = TRUE, include.dirs = FALSE)) # len-0 compatible # to remove previously built version
  
  if (!is.character(code_files) || !length(code_files) || anyNA(code_files) || !all(nzchar(code_files))) stop('illegal `code_files`')
  code_files <- file.path('./R', paste0(gsub(pattern = '\\.R$', replacement = '', x = code_files), '.R'))
  if (any(id <- !file.exists(code_files))) stop(sQuote(basename(code_files)[id]), ' no longer exist')
  
  suppressMessages(package.skeleton(name = name, path = path, force = TRUE, code_files = code_files, encoding = 'UTF-8'))
  unlink(file.path(pkg, 'man'), recursive = TRUE) # delete directory
  file.remove(file.path(pkg, 'Read-and-delete-me'))
  file.remove(file.path(pkg, 'NAMESPACE')) # \CRANpkg{roxygen2} will generate NAMESPACE
  system(paste0('open \'', normalizePath(pkg), '\'')) # for my debug
  
  if (length(rda_files)) {
    # allow vector `rda_files`; some data could be used in more than one packages
    if (!is.character(rda_files) || anyNA(rda_files) || !all(nzchar(rda_files))) stop('illegal `rda_files`')
    rda_files <- file.path('./data', paste0(gsub(pattern = '\\.rda$', replacement = '', x = rda_files), '.rda'))
    if (any(id <- !file.exists(rda_files))) stop(sQuote(basename(rda_files)[id]), ' no longer exist')
    dir.create(data_dir <- file.path(pkg, 'data'), showWarnings = FALSE)
    file.copy(from = rda_files, to = data_dir)
  }
  
  if (length(object2rda)) {
    dir.create(data_dir <- file.path(pkg, 'data'), showWarnings = FALSE)
    save(list = object2rda, file = file.path(data_dir, 'object2rda.rda'), compress = 'xz')
  }
  
  if (length(vignette_files)) {
    if (!is.character(vignette_files) || anyNA(vignette_files) || !all(nzchar(vignette_files))) stop('illegal `vignette_files`')
    vignette_files <- file.path('./vignettes', paste0(gsub(pattern = '\\.Rmd$', replacement = '', x = vignette_files), '.Rmd'))
    if (any(id <- !file.exists(vignette_files))) stop(sQuote(basename(vignette_files)[id]), ' no longer exist')
    dir.create(vignettes_dir <- file.path(pkg, 'vignettes'))
    file.copy(from = vignette_files, to = vignettes_dir)
  }
  
  if (!file.exists(DESC_file <- file.path(pkg, 'DESCRIPTION'))) stop('utils::package.skeleton updated?')
  dcf_raw <- read.dcf(file = DESC_file)
  cnm_deprecate <- c('Maintainer', 'Author') # derived from Author@R no later than Spring 2024
  dcf <- dcf_raw[, setdiff(colnames(dcf_raw), cnm_deprecate), drop = FALSE]
  dcf[, 'Title'] <- Title 
  # ?utils::package.skeleton uses ?base::Sys.time for 'Date' field
  # ?utils::package.skeleton hard-code 'Version: 1.0', as of 2023-06-16
  dcf[, 'Version'] <- Version
  if (length(Imports)) {
    if ('Imports' %in% colnames(dcf)) {
      # ?utils::package.skeleton may search for S4 definition and determines if an 'Imports' field will be created with entry of 'methods'.
      dcf[, 'Imports'] <- paste0(unique.default(c(dcf[, 'Imports'], Imports)), collapse = ', ')
    } else dcf <- cbind(dcf, 'Imports' = paste0(unique.default(Imports), collapse = ', '))
  }
  dcf[, 'Description'] <- gsub(pattern = '\\n', replacement = ' ', x = Description)
  dcf[, 'License'] <- 'GPL-2'
  manualField <- c('Authors@R', 'Language', 'Depends', 'Suggests', 'VignetteBuilder', 'LazyData')
  if (any(manualField %in% colnames(dcf))) stop('utils::package.skeleton updated?')
  dcf <- cbind(dcf, 
               'Authors@R' = deparse1(substitute(AuthorsR)),
               Language = 'en-US',
               Roxygen = 'list(markdown = TRUE)')
  # \pkg{roxygen2} writes its version to `RoxygenNote` field
  if (length(vignette_files)) dcf <- cbind(dcf, VignetteBuilder = 'knitr')
  if (length(rda_files) || length(object2rda)) dcf <- cbind(dcf, LazyData = 'true', LazyDataCompression = 'xz')
  
  # force to depend on latest R
  currentR <- paste0('R (>= ', R.version$major, '.', substr(R.version$minor, 1L, 1L), '.0)')
  # dont want to use `R.version.string`
  # https://stackoverflow.com/questions/48433412/cran-check-warning-dependence-on-r-version-3-4-3-not-with-patchlevel-0
  # Such dependencies should be made to the 'zero' level, i.e., R (>= 3.4.0)
  if (length(Depends) && any(id <- grepl('^R \\(.*\\)$', x = Depends))) {
    Depends[id] <- currentR # length(id) > 1L compatible
  } else Depends <- c(currentR, Depends)
  dcf <- cbind(dcf, Depends = paste(unique.default(Depends), collapse = ', ')) 
  
  if (length(vignette_files)) Suggests <- c(Suggests, 'knitr', 'rmarkdown')
  if (length(Suggests)) dcf <- cbind(dcf, Suggests = paste(unique.default(Suggests), collapse = ', '))
  write.dcf(dcf, file = DESC_file) # it's not read-only
  
  if (file.exists(NEWS <- paste0('NEWS_', name, '.md'))) {
    file.copy(from = NEWS, to = file.path(pkg, 'NEWS.md'))
  }
  
  return(invisible())
  
}



