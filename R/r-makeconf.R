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
  status <- sys::exec_wait(R, c("CMD", "config", VAR), std_out = outcon)
  output <- rawToChar(rawConnectionValue(outcon))
  if(identical(status, 0L)){
    strsplit(output, '\r?\n')[[1]]
  } else {
    # R CMD config seems to print errors to stdout :/
    stop(output, call. = FALSE)
  }
}

#' @export
#' @rdname r_cmd_config
#' @param cmd command to invoke (may be a variable)
#' @param args additional arguments for `cmd`
r_make_call <- function(cmd = '$(CC)', args = '--version'){
  makefile <- safe_path(system.file('run.make', package = 'makeconf'))
  args <- paste(args, collapse = " ")
  vars <- c(
    paste0('R_MAKECONF=', r_makeconf_path()),
    paste0('PROG=', cmd),
    paste0('ARGS=', args))
  sys::exec_internal('make', c('-f', makefile, vars), error = FALSE)
}

#' @export
#' @rdname r_cmd_config
#' @examples # Where your makeconf is stored:
#' r_makeconf_path()
r_makeconf_path <- function(){
  conf_path <- paste0(R.home('etc'), Sys.getenv('R_ARCH'), '/Makeconf')
  safe_path(conf_path)
}

safe_path <- function(x){
  x <- normalizePath(x, mustWork = TRUE)
  if(.Platform$OS.type == "windows"){
    x <- utils::shortPathName(x)
  }
  return(x)
}
