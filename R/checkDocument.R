
#' @title Check Document
#' 
#' @description ..
#' 
#' @param pkg \link[base]{character} scalar, directory of the package
#' 
#' @importFrom devtools build_manual document spell_check
#' @importFrom utils capture.output
#' @export
checkDocument <- function(pkg) {
  pkg <- normalizePath(pkg)
  name <- basename(pkg)
  path <- dirname(pkg)
  
  # system(paste('open', pkg)) # for my debug
  suppressMessages(document(pkg = pkg, roclets = c('rd', 'collate', 'namespace'), quiet = FALSE))
  # will add 'package:*name*' in base::search(), because of `load_code` parameter of ?roxygen2::roxygenize
  detach(name = paste0('package:', name), unload = TRUE, character.only = TRUE) # otherwise multiple S4 definitions
  
  Rd <- list.files(path = file.path(pkg, 'man'), pattern = '\\.Rd$', full.names = TRUE)
  lapply(Rd, FUN = function(i) { # (i = Rd[[11L]])
    inm <- basename(i)
    itxt <- readLines(con = i)
    
    idx <- grep('^\\\\docType\\{.*\\}$', x = itxt)
    if (n <- length(idx)) {
      if (n != 1L) stop('will not have more than one docType in one Rd file')
      i_docType <- gsub(pattern = '^\\\\docType\\{|\\}$', replacement = '', x = itxt[idx])
      switch(i_docType, data = {
        # do something
      }, package = { # '\\docType{package}' still created by \pkg{roxygen2}
        # do something
      }, class = { # S4 class
        # do something
      }, stop(sQuote(i_docType), ' unknown?'))
      return(invisible())
    }
    
    is_internal <- ('\\keyword{internal}' %in% itxt)
    
    if (!is_internal) {
      
      # check '\value' fields in .Rd files (@returns in \pkg{roxygen2})
      # \pkg{roxygen2} does not force this check, but CRAN personnel insists.
      idx <- ('\\value{' == itxt)
      if (sum(idx) > 1L) stop(sQuote(inm), ' has more than one @return')
      if (!any(idx)) stop(sQuote(inm), ' does not have @return') # masked temporarily..
      # no need; also do not want to import my [.cyan], etc.
      # cat(.cyan(c(sQuote(inm), 'has')), .bold(.red('@return')), .violet(sQuote(itxt[which(idx) + 1L])), '\n\n')
      
    }
    
    # After R 4.0.0, \dontest{} is always run as part of CRAN submission (see ?devtools::check)
    if (any('\\dontrun{' == itxt)) stop('Replace \\dontrun with \\donttest in ', sQuote(inm)) # masked temporarily
    #if (any(grepl('microbenchmark', x = itxt)) && !any('library(microbenchmark)' == itxt)) stop('microbenchmark in ', sQuote(inm))
  })
  
  print(spell_check(pkg = pkg, vignettes = FALSE)); cat('\n')
  
  capture.output(build_manual(pkg = pkg))
  version <- read.dcf(file = file.path(pkg, 'DESCRIPTION'), fields = 'Version')
  file.exists(pdf_manual <- file.path(path, paste0(name, '_', version, '.pdf')))
  system(paste('open', pdf_manual)) # no need to close the open pdf
}


