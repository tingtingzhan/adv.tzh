

#' @title Development Package Clean Up
#' 
#' @description
#' ..
#' 
#' @param pkg \link[base]{character} scalar
#' 
#' @param rm_o \link[base]{logical} scalar, whether to remove the `./src/*.o` files.
#' Default `FALSE`.
#' 
#' @details
#' Function [pkg_cleanup] prepares the working package `'.'` for compilation
#' \itemize{
#' \item {removes unrecognized files from main directory}
#' \item {removes size-0 `./R/*.R` and `./src/*.cpp` files}
#' \item {updates the *Date*, *Version* in DESCRIPTION file.
#' Always put S4 \link[methods]{setClass} and \link[methods]{setMethod} 
#' in a same file, then we do not need *Collate* in DESCRIPTION file}
#' }
#' 
#' @examples
#' if (FALSE) {
#'   pkg_cleanup('~/Dropbox/Packages/tzh') 
#' }
#' # # only in devtools::load_all('.') mode!!
#' 
#' @importFrom devtools as.package
#' @importFrom pkgload parse_deps
#' @export
pkg_cleanup <- function(pkg = '.', rm_o = FALSE) {
  
  pkg <- normalizePath(pkg)
  path <- dirname(pkg)
  name <- basename(pkg)
  
  #################### 
  # Remove files/directories
  
  # remove '*.Rcheck' folder and '*.tar.gz' files in the **parent** directory
  unlink(list.files(path = path, pattern = '\\.Rcheck$', full.names = TRUE), recursive = TRUE)
  unlink(list.files(path = path, pattern = '\\.tar\\.gz$', full.names = TRUE))
  
  # unrecognizable files
  file.remove(list.files(path = pkg, pattern = '\\.txt$|\\.docx$|\\.doc$|\\.xlsx$|\\.xls$|\\.csv$|\\.pdf$|\\.html$|\\.png$|\\.bmp$|\\.tiff$|\\.rda$|\\.rds$|\\.log$|\\.rmd$', all.files = TRUE))
  # pattern = '\\.md$' is *not* to be removed (e.g., 'NEWS.md', 'NEWS_*SubPkg*.md')
  
  #if (length(list.files(path = path, pattern = 'conflicted copy', all.files = TRUE))) stop('Remove conflicted sync files by hand!')
  
  # size-0 files
  fR <- list.files(path = file.path(pkg, 'R'), full.names = TRUE)
  fsrc <- list.files(path = file.path(pkg, 'src'), full.names = TRUE)
  fcode <- c(fR, fsrc)
  if (any(grepl(pattern = 'conflicted copy', x = fcode))) stop('Dropbox conflict in \\r, \\demo and/or \\src.')
  file.remove(fcode[file.size(fcode) == 0]) # remove size-0 code files, len-0 compatible
  
  # ./src/*.o files
  if (rm_o) {
    file.remove(list.files(path = file.path(pkg, 'src'), pattern = '\\.so$|\\.o$|\\.dll$', full.names = TRUE))
    file.remove(list.files(path = file.path(pkg, 'src-x64'), full.names = TRUE)) # non-existent `path` returns len-0 files
    file.remove(list.files(path = file.path(pkg, 'src-x386'), full.names = TRUE))
    file.remove(list.files(path = file.path(pkg, 'src-i386'), full.names = TRUE))
  }
  
  # delete non-empty directories (exist or not)
  unlink(file.path(pkg, c('results', 'lib', 'rmd', 'html', 'word', 'tmp')), recursive = TRUE) 
  # './results' (deprecated) created by my ?writeRproj_*
  # './lib' created by ?htmltools::save_html
  # './rmd' (deprecated)
  # './html' and './word' created by ?renderRproj
  # './tmp' (deprecated) created by files in ./demo/learn_*.R
  
  # ALREADY DONE!!!!
  # ../*.Rcheck directory and ../*.tar.gz files
  #dir_Rcheck <- sprintf(fmt = '../%s.Rcheck', name) # paste0('../*.Rcheck') does not work!!
  #unlink(dir_Rcheck, recursive = TRUE) # delete non-empty directories
  #file.remove(list.files('../', pattern = paste0('^', name, '.*\\.tar\\.gz$'), full.names = TRUE))
  # end of ALREADY DONE!!!!
  
  # opens the file for writing, and closes it immediately
  # Then the file stays, but wiped empty.
  close.connection(file(file.path(pkg, '.Rhistory'), open = 'wt'))
  
  #################### 
  # update ./DESCRIPTION
  
  desc <- read.dcf(file = file.path(pkg, 'DESCRIPTION'))
  desc[, 'Date'] <- format.Date(Sys.Date())
  desc[, 'Version'] <- '0.1.0'
  
  desc[, 'Collate'] <- paste0('\'', list.files(file.path(pkg, 'R'), full.names = FALSE, pattern = '\\.R$'), '\'', collapse = ' ')
  # learn more about ?roxygen2::update_collate
  # dont mask yet..
  
  .depends <- sort.int(unique.default(parse_deps(desc[, 'Depends'])$name)) # \pkg{ggplot2} in 'Depends'
  .imports <- sort.int(unique.default(parse_deps(desc[, 'Imports'])$name))
  .suggests <- sort.int(unique.default(parse_deps(desc[, 'Suggests'])$name))
  if (anyDuplicated.default(c(.depends, .imports, .suggests))) stop('cannot have duplicated package(s) in `Depends`, `Imports` and `Suggests`')
  pkg_installed(c(.depends, .imports, .suggests)) # STOP on deprecated/github Imports/Suggests (neither on CRAN nor Bioconductor)
  depends_R <- sprintf(fmt = 'R (>= %s.%s)', R.version$major, strsplit(R.version$minor, split = '\\.')[[1L]][1L])  # https://github.com/r-lib/devtools/issues/1742
  desc[, 'Depends'] <- paste(c(depends_R, .depends), collapse = ', ')
  desc[, 'Imports'] <- paste(.imports, collapse = ', ')
  desc[, 'Suggests'] <- paste(.suggests, collapse = ', ')
  
  write.dcf(desc, file = file.path(pkg, 'DESCRIPTION')) # just write, no matter updated or not
  
  #################### 
  # update ./demo/00Index
  #demo_full <- list.files('./demo', full.names = TRUE, pattern = '\\.R$')
  #if (!all(grepl(pattern = '\\.R$', x = demo_full))) stop('demo folder has files not with extension of .R')
  #fdemo <- gsub('\\.R$', replacement = '', x = basename(demo_full))
  #demo_txt <- vapply(demo_full, FUN = function(i) gsub(pattern = '^# ', replacement = '', x = readLines(con = i)[1L]), FUN.VALUE = '')
  #writeLines(paste0(fdemo, '\t', demo_txt), con = './demo/00Index')
  
  if (dir.exists(file.path(pkg, 'demo'))) {
    stop('./demo is to be deprecated.  see https://r-pkgs.org/misc.html')
  }
  
}


