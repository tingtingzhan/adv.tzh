

# RStudio do not have a delete button for base-packages
installed.packages(priority = 'base') |> rownames() 

installed.packages(priority = 'recommended') |> rownames()

getOption('defaultPackages') # what is this ??

cran = available.packages() |> as.data.frame.matrix()
nrow(cran) # 22501, 2025-07-30
table(cran[, 'Priority'], useNA = 'ifany')
cran |> subset(subset = (Priority == 'optional')) # what is this ??

# ?BiocManager::available # includes CRAN packages, and very slow..
Bioc = 'https://bioconductor.org/packages/release/bioc/src/contrib/PACKAGES' |>
  url() |>
  read.dcf(fields = 'Package') |> # 'matrix'
  c()
length(Bioc) # 2309, 2025-07-30 
stopifnot(!length(intersect(rownames(cran), Bioc)))
?BiocManager::repositories 
?BiocManager:::.repositories


# update at least every month.



# latest installed packages on hard drive
installed.packages() |> # matrix
  rownames() |>
  setNames(nm = _) |>
  lapply(FUN = packageDate) |>
  do.call(what = c, args = _) |>
  sort(decreasing = TRUE) |>
  head(n = 20L)

