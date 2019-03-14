#' Query pkg-config
#'
#' Wrappers for the pkg-config utility to query information on C/C++
#' libraries installed on your system.
#'
#' @export
#' @rdname pkgconfig
#' @examples # Where your makeconf is stored:
#' pkg_config_info()
pkg_config_info <- function(){
  name <- pkgconfig_path()
  path <- unname(Sys.which(name))
  version <- if(nchar(path)){
    pkg_config_call('--version')
  }
  pc_path <- if(nchar(path)){
    pkg_config_call(c('--variable', 'pc_path', 'pkg-config'))
  }
  list(
    name = name,
    path = path,
    version = version,
    pc_path = pc_path,
    pkg_config_path = Sys.getenv('PKG_CONFIG_PATH')
  )
}

#' @export
#' @rdname pkgconfig
#' @examples pkg_list_all()
pkg_list_all <- function(){
  txt <- pkg_config_call('--list-all')
  names <- gsub('\\s+.*', '', txt)
  desc <- gsub('^\\S+\\s+', '', txt)
  srt <- order(names)
  df <- data.frame (pkg = names, description = desc, stringsAsFactors = FALSE)
  df_as_tibble(df[order(names),])
}

#' @export
#' @rdname pkgconfig
pkg_exists <- function(pkg = 'libcurl'){
  sys::exec_wait(pkgconfig_path(), c('--exists', pkg)) == 0
}

#' @export
#' @rdname pkgconfig
pkg_version <- function(pkg = 'libcurl'){
  as.numeric_version(pkg_config_call(c('--modversion', pkg)))
}

#' @export
#' @rdname pkgconfig
#' @param pkg names of the pkg-config libraries to query
pkg_cflags <- function(pkg = 'libcurl'){
  pkg_config_call(c('--cflags', pkg))
}

#' @export
#' @rdname pkgconfig
#' @param static get libs for static linking, i.e. include dependencies
pkg_libs <- function(pkg = 'libcurl', static = FALSE){
  pkg_config_call(c("--libs", if(isTRUE(static)) "--static", pkg))
}

#' @export
#' @rdname pkgconfig
pkg_info <- function(pkg = 'libcurl'){
  list (
    version = pkg_version(pkg),
    cflags = pkg_cflags(pkg),
    libs = pkg_libs(pkg)
  )
}

pkgconfig_path <- function(){
  Sys.getenv('PKG_CONFIG_PATH', paste0(Sys.getenv('BINPREF'), 'pkg-config'))
}

pkg_config_call <- function(args){
  # For now, we call via make on Windows to support msys2 paths
  out <- if(is_windows()){
    make_call(pkgconfig_path(), args)
  } else {
    sys::exec_internal(pkgconfig_path(), args, error = FALSE)
  }
  if(out$status != 0){
    stop(rawToChar(out$stderr), call. = FALSE)
  }
  as_text(out$stdout)
}

df_as_tibble <- function(df){
  stopifnot(is.data.frame(df))
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}

is_windows <- function(){
  .Platform$OS.type == 'windows'
}
