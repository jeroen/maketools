#' Check your makeconf
#'
#' Wrappers for `make` and `R CMD config` to lookup and test make variables
#' from your local `Makeconf` file.
#'
#' @export
#' @rdname r_cmd_config
#' @param VAR value passed to `R CMD config` such as `CXX` or `FC`
r_cmd_config <- function(VAR = '--all'){
  R <- file.path(R.home('bin'), 'R')
  outcon <- rawConnection(raw(0), "r+")
  on.exit(close(outcon), add = TRUE)
  out <- sys::exec_internal(R, c("CMD", "config", VAR), error = FALSE)
  if(out$status == 0){
    as_text(out$stdout)
  } else {
    stop(as_text(c(out$stdout, out$stderr)), call. = FALSE)
  }
}

#' @export
#' @rdname r_cmd_config
#' @param cmd command to invoke (may be a variable)
#' @param args additional arguments for `cmd`
r_make_call <- function(cmd = '$(CC)', args = '--version'){
  testmake <- ifelse(is_solaris(), 'solaris.make', 'test.make')
  makefile <- safe_path(system.file(testmake, package = 'makeconf'))
  args <- paste(args, collapse = " ")
  vars <- c(
    paste0('R_MAKECONF=', r_makeconf_path()),
    paste0('PROG=', cmd),
    paste0('ARGS=', args))
  r_exec_make(c('-f', makefile, vars))
}

#' @export
#' @rdname r_cmd_config
#' @examples # Where your makeconf is stored:
#' r_make_info()
r_make_info <- function(){
  name <- r_make_path()
  path <- unname(Sys.which(r_make_path()))
  version <- if(nchar(path)){
    info <- r_exec_make('--version')
    if(info$status == 0){
      as_text(info$stdout)[1]
    }
  }
  list(
    name = name,
    path = path,
    version = version,
    makeconf = r_makeconf_path()
  )
}

r_exec_make <- function(args){
  sys::exec_internal(r_make_path(), args, error = FALSE)
}

r_makeconf_path <- function(){
  conf_path <- paste0(R.home('etc'), Sys.getenv('R_ARCH'), '/Makeconf')
  safe_path(conf_path)
}

r_make_path <- function(){
  Sys.getenv('MAKE', 'make')
}

safe_path <- function(x){
  x <- normalizePath(x, mustWork = TRUE)
  if(.Platform$OS.type == "windows"){
    x <- utils::shortPathName(x)
  }
  return(x)
}

as_text <- function(x){
  if(length(x)){
    con <- rawConnection(x)
    on.exit(close(con))
    readLines(con)
  }
}

is_solaris <- function(){
  Sys.info()[["sysname"]] == "SunOS"
}
