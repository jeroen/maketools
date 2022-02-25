#' Package tools
#'
#' Get some extra info about packages.
#'
#' @export
#' @rdname extra
#' @param path root directory of package
find_logo <- function (path = ".") {
  # Match logic from pkgdown but return path relative package root.
  files <- c('logo.svg', 'man/figures/logo.svg', 'logo.png', 'man/figures/logo.png')
  candidates <- file.path(path, files)
  utils::head(files[file.exists(candidates)], 1)
}
