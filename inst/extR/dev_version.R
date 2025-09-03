
# all packages on hard drive that are later than cran version

ap = available.packages() |>
  as.data.frame()

pkg = intersect(
  ap |> rownames(),
  installed.packages() |> rownames()
)
length(pkg)

pkg |>
  setNames(nm = _) |>
  vapply(FUN = \(i) {
    local = i |>
      packageVersion()
    cran = ap |>
      subset(subset = (Package == i), select = 'Version', drop = TRUE) |>
      as.package_version()
    local > cran
  }, FUN.VALUE = NA) |>
  which()

pkg |>
  setNames(nm = _) |>
  vapply(FUN = \(i) {
    local = i |>
      packageVersion()
    cran = ap |>
      subset(subset = (Package == i), select = 'Version', drop = TRUE) |>
      as.package_version()
    local < cran
  }, FUN.VALUE = NA) |>
  which()


