#' Show System Dependencies
#'
#' Finds the shared libraries that an installed package links to by running `ldd`
#' on the package `so` file. Then uses system package manager (i.e. `dpkg` or `rpm`)
#' to find the rpm/deb packages that contain the libs and the headers for this library.
#'
#' @export
#' @rdname sysdeps
#' @name sysdeps
#' @param pkg name of an installed packages
#' @param lib.loc path to the library of the package
package_sysdeps <- function(pkg, lib.loc = NULL){
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
  pkgs <- find_packages(paths)
  package <- get_names(pkgs)
  version <- get_versions(pkgs)
  headerpkg <- get_names(find_packages(sub(".so[.0-9]+$", ".so", paths)))
  data.frame(
    shlib = basename(paths),
    package = package,
    headers = headerpkg,
    version = version,
    url = get_package_urls(package),
    stringsAsFactors = FALSE
  )
}

#' @export
#' @rdname sysdeps
package_sysdeps_string <- function(pkg, lib.loc = NULL){
  df <- package_sysdeps(pkg = pkg, lib.loc = lib.loc)
  paste0(sprintf("%s (%s)", df$package, df$version), collapse = ", ")
}

get_names <- function(str){
  vapply(strsplit(str, "\t", fixed = TRUE), FUN = head, character(1), n = 1, USE.NAMES = FALSE)
}

get_versions <- function(str){
  vapply(strsplit(str, "\t", fixed = TRUE), FUN = tail, character(1), n = 1, USE.NAMES = FALSE)
}

find_packages <- function(paths){
  switch(pkg_format(),
         dpkg = vapply(paths, dpkg_find_anywhere, character(1), USE.NAMES = FALSE),
         rpm = vapply(paths, rpm_find, character(1), USE.NAMES = FALSE),
         rep(NA, length(paths))
  )
}

rpm_find <- function(path){
  tryCatch(sys_call('rpm', c('-qf', path, '--qf', "%{NAME}\t%{VERSION}\n")), error = function(e){
    NA_character_
  })
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
  sys_call('dpkg-query', c('-f', '${Package}\t${Version}\n',"--show", fullpkg))
}

get_disto <- function(){
  tryCatch(sys_call("lsb_release", "-sc"), error = function(e){
    # If lsb_release is unavailable
    system("dpkg --status tzdata | grep Provides|cut -f2 -d'-'", intern = TRUE)
  })
}

sys_call <- function(cmd, args = NULL){
  sys::as_text(sys::exec_internal(cmd = cmd, args = args)$stdout)
}

pkg_format <- function(){
  if(has('dpkg') && has('apt'))
    return("dpkg")
  if(has('rpm')  && any(has(c('dnf', 'yum'))))
    return('rpm')
  NA_character_
}

has <- function(x){
  nchar(Sys.which(x)) > 0
}

get_package_urls <- function(pkgs){
  os <- utils::sessionInfo()$running
  if(grepl("ubuntu", os, ignore.case = TRUE)){
    sprintf('https://packages.ubuntu.com/%s/%s', get_disto(), pkgs)
  } else if(grepl("debian", os, ignore.case = TRUE)){
    sprintf('https://packages.debian.org/%s/%s', get_disto(), pkgs)
  } else if(grepl("fedora", os, ignore.case = TRUE)) {
    sprintf('https://src.fedoraproject.org/rpms/%s', pkgs)
  } else {
    NA_character_
  }
}
