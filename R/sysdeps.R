#' Show System Dependencies
#'
#' Finds the shared libraries that an installed package links to by running `ldd`
#' on the package `so` file. Then uses `dpkg` find the debian packages that contain
#' the libs and the headers for this library.
#'
#' @export
#' @rdname sysdeps
#' @name sysdeps
#' @param pkg name of an installed packages
#' @param lib.loc path to the library of the package
dpkg_sysdeps <- function(pkg, lib.loc = NULL){
  pkgpath <- system.file(package = pkg, lib.loc = lib.loc)
  if(!nchar(pkgpath))
    stop("Package not found")
  dll <- file.path(pkgpath, sprintf('libs/%s.so', pkg))
  if(!file.exists(dll)) # No compiled code
    return(NULL)
  lddinfo <- sys_call('ldd', dll)
  lddinfo <- sub(" \\([a-f0-9x]+\\)$", "", lddinfo)
  text <- sys_call('readelf', c('-d', dll))
  text <- grep('^.*NEEDED.*\\[(.*)\\]$', text, value = TRUE)
  shlibs <- sub('^.*NEEDED.*\\[(.*)\\]$', '\\1', text)
  paths <- lapply(shlibs, function(x){
    name <- strsplit(x, '.', fixed = TRUE)[[1]][1]
    if(isTRUE(name %in% c("libR", "libm", "libgcc_s", "libc", "ld-linux-x86-64")))
      return(NULL) # R itself already depends on these
    line <- grep(x, lddinfo, fixed = TRUE, value = TRUE)
    utils::tail(strsplit(line, ' ', fixed = TRUE)[[1]], 1)
  })
  paths <- trimws(unlist(paths))
  pkg_run <- vapply(paths, dpkg_find_anywhere, character(1), USE.NAMES = FALSE)
  package <- vapply(pkg_run, dpkg_get_name, character(1), USE.NAMES = FALSE)
  libs <- sub(".so[.0-9]+$", ".so", paths)
  pkg_dev <- vapply(libs, dpkg_find_anywhere, character(1), USE.NAMES = FALSE)
  data.frame(
    file = basename(paths),
    package = package,
    headers = vapply(pkg_dev, dpkg_get_name, character(1), USE.NAMES = FALSE),
    version = vapply(pkg_run, dpkg_get_version, character(1), USE.NAMES = FALSE),
    url = get_package_urls(package),
    stringsAsFactors = FALSE
  )
}

#' @export
#' @rdname sysdeps
dpkg_sysdeps_string <- function(pkg, lib.loc = NULL){
  df <- dpkg_sysdeps(pkg = pkg, lib.loc = lib.loc)
  paste0(sprintf("%s (%s)", df$package, df$version), collapse = ", ")
}

dpkg_get_name <- function(str){
  head(strsplit(str, "[\t:]")[[1]], 1)
}

dpkg_get_version <- function(str){
  tail(strsplit(str, "\t", fixed = TRUE)[[1]], 1)
}

# In Debian /usr/lib and /lib are both used sometimes, we need to check both
dpkg_find_anywhere <- function(path){
  tryCatch(dpkg_find(path), error = function(e){
    path <- sub("^/usr/usr", "", paste0('/usr', path))
    tryCatch(dpkg_find(path), error = function(e){
      #message(e)
      NA_character_
    })
  })
}

dpkg_find <- function(path){
  info <- sys_call('dpkg', c('-S', path))
  fullpkg <- strsplit(info, ":? ")[[1]][1]
  sys_call('dpkg-query', c("--show", fullpkg))
}

get_disto <- function(){
  sys_call("lsb_release", "-sc")
}

sys_call <- function(cmd, args = NULL){
  sys::as_text(sys::exec_internal(cmd = cmd, args = args)$stdout)
}

get_package_urls <- function(pkgs){
  os <- utils::sessionInfo()$running
  distro <- get_disto()
  if(grepl("ubuntu", os, ignore.case = TRUE)){
    sprintf('https://packages.ubuntu.com/%s/%s', distro, pkgs)
  } else if(grepl("debian", os, ignore.case = TRUE)){
    sprintf('https://packages.debian.org/%s/%s', distro, pkgs)
  } else {
    stop("Unknown os:", os)
  }
}
