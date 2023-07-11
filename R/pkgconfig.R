#' Query pkg-config
#'
#' Wrappers for the pkg-config utility to query information on C/C++ libraries
#' that are available on your system.
#'
#' @export
#' @family maketools
#' @rdname pkgconfig
#' @name pkgconfig
#' @examples # Check if pkg-config is available
#' (info <- pc_info())
#' if(info$available)
#'   pc_pkg_list()
pc_info <- function(){
  name <- pkgconfig_name()
  path <- pkgconfig_path(name, error = FALSE)
  version <- if(is_string(path)){
    pkg_config_call('--version')
  }
  pc_path <- if(is_string(path)){
    # Solaris has super old pkg-config that doesn't support this
    tryCatch(pkg_config_call(c('--variable', 'pc_path', 'pkg-config')), error = function(e){NA})
  }
  list(
    name = name,
    path = path,
    version = version,
    pc_path = pc_path,
    available = is_string(path),
    pkg_config_path = Sys.getenv('PKG_CONFIG_PATH')
  )
}

#' @export
#' @rdname pkgconfig
pc_pkg_list <- function(){
  txt <- pkg_config_call('--list-all')
  names <- gsub('\\s+.*', '', txt)
  desc <- gsub('^\\S+\\s+', '', txt)
  srt <- order(names)
  df <- data.frame (pkg = names[srt], description = desc[srt], stringsAsFactors = FALSE)
  df_as_tibble(df)
}

#' @export
#' @rdname pkgconfig
pc_pkg_exists <- function(pkg = 'libcurl'){
  sys::exec_wait(pkgconfig_path(), c('--exists', pkg)) == 0
}

#' @export
#' @rdname pkgconfig
pc_pkg_version <- function(pkg = 'libcurl'){
  as.numeric_version(pkg_config_call(c('--modversion', pkg)))
}

#' @export
#' @rdname pkgconfig
#' @param pkg names of the pkg-config libraries to query
pc_pkg_cflags <- function(pkg = 'libcurl'){
  pkg_config_call(c('--cflags', pkg))
}

#' @export
#' @rdname pkgconfig
#' @param static get libs for static linking, i.e. include dependencies
pc_pkg_libs <- function(pkg = 'libcurl', static = FALSE){
  pkg_config_call(c("--libs", if(isTRUE(static)) "--static", pkg))
}

#' @export
#' @rdname pkgconfig
pc_pkg_info <- function(pkg = 'libcurl'){
  list (
    version = pc_pkg_version(pkg),
    cflags = pc_pkg_cflags(pkg),
    libs = pc_pkg_libs(pkg)
  )
}

pkgconfig_name <- function(){
  pc <- Sys.getenv('PKG_CONFIG', "")
  if(is_string(pc))
    return(pc)
  ifelse(is_windows(), make_echo('$(BINPREF)pkg-config.exe'), 'pkg-config')
}

pkgconfig_path <- function(name = pkgconfig_name(), error = TRUE){
  pc <- lookup_path(name)
  if(!is_string(pc) && isTRUE(error))
    stop("pkg-config is not available on this system", call. = FALSE)
  return(pc)
}

lookup_path <- function(name){
  out <- if(is_windows() && grepl("^/", name)){
    as_text(make_call('/bin/cygpath', c('-m', name))$stdout)
  } else {
    unname(Sys.which(name))
  }
  if(is_string(out)){
    normalizePath(out)
  } else {
    return(NA)
  }
}

pkg_config_call <- function(args){
  pc <- pkgconfig_path()
  out <- sys::exec_internal(pc, args, error = FALSE)
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
