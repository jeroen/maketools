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

#' @export
#' @rdname extra
#' @param pkg name of the installed package
vignettes_base64 <- function(pkg){
  vignettes <- as.data.frame(tools::getVignetteInfo(pkg))
  if(nrow(vignettes) > 0){
    df <- vignettes[c('File', 'PDF', 'Title')]
    names(df) <- c("source", "filename", "title")
    gsub("\n", "", jsonlite::base64_enc(jsonlite::toJSON(df)), fixed = TRUE)
  }
}
