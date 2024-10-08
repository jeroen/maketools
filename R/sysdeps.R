#' Package System Dependencies
#'
#' Shows the external shared libraries that an installed R package is linked to
#' by running `ldd` on the package `so` file. Then uses system package manager
#' (e.g. `dpkg` or `rpm` or `brew`) to locate which system package that contains
#' the binaries, headers, and (if available) sources for this library.
#'
#' For common distributions, the output also includes a URL to the distro-homepage
#' of the system package. Here we can typically find more information about the
#' package, such as configuration options, dependencies, and custom patches applied
#' by your distribution.
#'
#' Because we use `ldd`, this only shows run-time dependencies of an installed R
#' package. This is especially relevant if you distribute the compiled R package
#' in binary form, because the same external libraries need to be available on
#' the user/deployment machine. This tool does not show dependencies that are
#' only needed at build-time, such as static or header-only libraries, and other
#' utilities required to build the package.
#'
#' @export
#' @family maketools
#' @rdname sysdeps
#' @name sysdeps
#' @param pkg name of an installed R package
#' @param lib.loc path to the R package directory for this package
package_sysdeps <- function(pkg, lib.loc = NULL){
  if(running_on('windows'))
    stop("This function currently does not work on Windows.")
  paths <- package_links_to(pkg = pkg, lib.loc = lib.loc)
  pkgdll <- as.character(attr(paths, 'pkgdll'))
  paths <- Filter(function(dllpath){
    !identical(site_dir(dllpath), site_dir(pkgdll))
  }, paths)
  skiplist <- c("libR", "libRblas", "libRlapack", "libm", "libgcc_s", "libc", "ld-linux-x86-64", "libSystem.B")
  paths <- paths[is.na(match(dll_name_only(paths), skiplist))]
  paths <- normalizePath(paths, mustWork = FALSE) # expand symlinks
  paths_to_sysdep(paths)
}

paths_to_sysdep <- function(paths){
  pkgs <- find_packages(paths)
  df <- data.frame(
    shlib = basename(paths),
    package = get_names(pkgs),
    headers = get_names(find_packages(strip_so_version(paths))),
    source = get_source(pkgs),
    version = get_versions(pkgs),
    url = get_package_urls(pkgs),
    stringsAsFactors = FALSE
  )
  df_as_tibble(df)
}

#dll_has_symbol <- function(dll, symbol){
#  db <- tools:::read_symbols_from_object_file(dll)
#  symbol %in% db[,'name']
#}

#' @export
#' @rdname sysdeps
package_sysdeps_string <- function(pkg, lib.loc = NULL){
  df <- package_sysdeps(pkg = pkg, lib.loc = lib.loc)
  df <- subset(df, !is.na(df$package))
  paste0(sprintf("%s (%s)", df$package, df$version), collapse = ", ")
}

#' @export
#' @rdname sysdeps
package_links_to <- function(pkg, lib.loc = NULL){
  pkgpath <- system.file(package = pkg, lib.loc = lib.loc)
  if(!nchar(pkgpath))
    stop("Package not found")
  dll <- file.path(pkgpath, sprintf('libs%s/%s%s', Sys.getenv('R_ARCH'), pkg, .Platform$dynlib.ext))
  if(!file.exists(dll)) # No compiled code
    return(character())

  # Todo: is there a better way to detect rust?
  # Does not work icw _R_SHLIB_STRIP_=true
  #if(running_on('ubuntu') && dll_has_symbol(dll, '_rust_begin_unwind')){
  #  rustc <- Sys.which('rustc')
  #  if(nchar(rustc)){
  #    return(rustc)
  #  }
  #}
  structure(if(running_on('macos')){
    links_to_macos(dll)
  } else if(running_on('windows')) {
    links_to_windows(dll)
  } else {
    links_to_ldd(dll)
  }, pkgdll = dll)
}

links_to_ldd <- function(dll){
  readelf <- ifelse(running_on('solaris'), 'greadelf', 'readelf')
  lddinfo <- sys_call('ldd', dll, error = FALSE)
  lddinfo <- sub(" \\([a-f0-9x]+\\)$", "", lddinfo)
  text <- sys_call(readelf, c('-d', dll))
  text <- grep('^.*NEEDED.*\\[(.*)\\]$', text, value = TRUE)
  shlibs <- sub('^.*NEEDED.*\\[(.*)\\]$', '\\1', text)
  paths <- lapply(shlibs, function(x){
    line <- grep(x, lddinfo, fixed = TRUE, value = TRUE)
    if(grepl('not found', line, fixed = TRUE)){
      return(x)
    }
    utils::tail(strsplit(line, ' ', fixed = TRUE)[[1]], 1)
  })
  trimws(unlist(paths))
}

links_to_windows <- function(dll){
  lddinfo <- sys_call('ldd', dll, error = FALSE)
  lddinfo <- sub(" \\([a-f0-9x]+\\)$", "", lddinfo)
  out <- vapply(strsplit(lddinfo, '=>', fixed = TRUE), function(x){
    trimws(utils::tail(x, 1))
  }, character(1), USE.NAMES = FALSE)
  out[!grepl('^../windows/', out, ignore.case = TRUE)]
}

links_to_macos <- function(dll){
  lddinfo <- sys_call('otool', c('-L', dll))
  m <- regexpr("/.*\\.(so|dylib)", lddinfo)
  Filter(function(x) {
    !identical(x, dll)
  }, regmatches(lddinfo, m))
}

dll_name_only <- function(path){
  tools::file_path_sans_ext(strip_so_version(basename(path)))
}

strip_so_version <- function(dll){
  sub("(\\.\\d+)*$", '', dll)
}

get_names <- function(str){
  vapply(strsplit(str, "\t", fixed = TRUE), function(x){
    x[1]
  }, character(1), USE.NAMES = FALSE)
}

get_versions <- function(str){
  vapply(strsplit(str, "\t", fixed = TRUE), function(x){
    x[2]
  }, character(1), USE.NAMES = FALSE)
}

get_source <- function(str){
  vapply(strsplit(str, "\t", fixed = TRUE), function(x){
    pkg_parse_name(x[3])
  }, character(1), USE.NAMES = FALSE)
}

pkg_parse_name <- function(pkg){
  strsplit(pkg, '-\\d')[[1]][1]
}

find_packages <- function(paths){
  switch(pkg_format(),
         dpkg = vapply(paths, dpkg_find_anywhere, character(1), USE.NAMES = FALSE),
         rpm = vapply(paths, rpm_find, character(1), USE.NAMES = FALSE),
         apk = vapply(paths, apk_find, character(1), USE.NAMES = FALSE),
         pacman = vapply(paths, pacman_find, character(1), USE.NAMES = FALSE),
         brew = vapply(paths, brew_find, character(1), USE.NAMES = FALSE),
         rep(NA_character_, length(paths))
  )
}

brew_find <- function(path){
  path <- normalizePath(path) # expands symlink
  pattern <- '/usr/local/Cellar/([^/]+)/([^/]+)/.*'
  if(grepl(pattern, path)){
    pkgname <- sub(pattern, '\\1\t\\2', path)
  } else {
    NA_character_
  }
}

apk_find <- function(path){
  tryCatch({
    out <- sys_call('apk', c('info', '--who-owns', path))
    pkg <- utils::tail(strsplit(out, ' ', fixed = TRUE)[[1]], 1)
    name <- pkg_parse_name(pkg)
    version <- sub(paste0(name, '-'), '', pkg, fixed = TRUE)
    paste(name, version, sep = '\t')
  }, error = function(e){
    NA_character_
  })
}

rpm_find <- function(path){
  tryCatch(sys_call('rpm', c('-qf', path, '--qf', "%{NAME}\t%{VERSION}\t%{SOURCERPM}\n")), error = function(e){
    NA_character_
  })
}

pacman_find <- function(path){
  tryCatch({
    str <- sys_call('pacman', c('-Qo', path))
    paste(utils::tail(strsplit(str, ' ', fixed = TRUE)[[1]], 2), collapse = '\t')
  }, error = function(e){
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
  sys_call('dpkg-query', c('-f', '${Package}\t${Version}\t${source:Package}\n',"--show", fullpkg))
}

get_disto <- function(){
  tryCatch(sys_call("lsb_release", "-sc"), error = function(e){
    # If lsb_release is unavailable
    system("dpkg --status tzdata | grep Provides|cut -f2 -d'-'", intern = TRUE)
  })
}

sys_call <- function(cmd, args = NULL, error = TRUE){
  sys::as_text(sys::exec_internal(cmd = cmd, args = args, error = error)$stdout)
}

pkg_format <- function(){
  if(running_on('macos'))
    return('brew')
  if(has('dpkg') && has('apt'))
    return("dpkg")
  if(has('rpm')  && any(has(c('dnf', 'yum'))))
    return('rpm')
  if(has('apk'))
    return('apk')
  if(has('pacman'))
    return('pacman')
  NA_character_
}

has <- function(x){
  nchar(Sys.which(x)) > 0
}

get_apk_repo <- function(pkg_names){
  vapply(pkg_names, function(pkg){
    tryCatch({
      text <- sys_call('apk', c('policy', pkg))
      url <- grep('https?://', text, value = TRUE)
      stopifnot(length(url) > 0)
      repo <- basename(url)
      version <- basename(dirname(url))
      paste(version, repo, sep = '/')
    }, error = function(e){'edge/main'})
  }, character(1), USE.NAMES = FALSE)
}

get_pacman_repo <- function(pkg_names){
  vapply(pkg_names, function(pkg){
    tryCatch({
      text <- sys_call('pacman', c('-Si', pkg))
      repo <- grep('^Repository', text, value = TRUE)[1]
      stopifnot(length(repo) > 0)
      utils::tail(strsplit(repo, ' ', fixed = TRUE)[[1]], 1)
    }, error = function(e){'core'})
  }, character(1), USE.NAMES = FALSE)
}

get_brew_url <- function(pkg_names){
  vapply(pkg_names, function(pkg){
    if(is.na(pkg))
      return(NA_character_)
    tryCatch({
      info <- sys_call('brew', c("info", pkg))
      pattern <- "^From: (.*)$"
      text <- grep(pattern, info, value = TRUE)
      stopifnot(length(text) > 0)
      sub(pattern, '\\1', text)
    }, error = function(e){
      prefix <- substring(pkg, 1, ifelse(grepl("^lib", pkg), 3, 1))
      sprintf('https://github.com/homebrew/homebrew-core/blob/master/Formula/%s/%s.rb', prefix, pkg)
    })
  }, character(1), USE.NAMES = FALSE)
}

get_package_urls <- function(pkgs){
  os <- utils::sessionInfo()$running
  pkg_names <- get_names(pkgs)
  out <- if(running_on('macos')){
    get_brew_url(pkg_names)
  } else if(running_on("ubuntu")){
    sprintf('https://packages.ubuntu.com/%s/%s', get_disto(), pkg_names)
  } else if(running_on("debian")){
    sprintf('https://packages.debian.org/%s/%s', get_disto(), pkg_names)
  } else if(running_on("fedora")) {
    sprintf('https://src.fedoraproject.org/rpms/%s', get_source(pkgs))
  } else if(running_on("alpine")) {
    sprintf('https://pkgs.alpinelinux.org/package/%s/x86_64/%s', get_apk_repo(pkg_names), pkg_names)
  } else if(running_on("arch")) {
    repos <- get_pacman_repo(pkg_names)
    sprintf("https://www.archlinux.org/packages/%s/x86_64/%s", repos, pkg_names)
  } else {
    rep(NA_character_, length(pkgs))
  }
  ifelse(is.na(pkgs), NA_character_, out)
}

running_on <- function(str){
  isTRUE(grepl(str, utils::sessionInfo()$running, ignore.case = TRUE))
}

site_dir <- function(x){
  tolower(normalizePath(dirname(dirname(x)), mustWork = FALSE))
}
