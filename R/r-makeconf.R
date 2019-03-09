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
#' @param args argument(s) to test program
make_test <-function(VAR, args = '--version'){
  PATH <- r_cmd_config(VAR)
  info <- make_call(PATH, args)
  version <- if(length(info$stdout)){
    con <- rawConnection(info$stdout)
    on.exit(close(con))
    readLines(con)
  }

  list(
    path = PATH,
    available = info$status == 0,
    version = version
  )
}

make_call <- function(cmd = '$(CC)', args = '--version'){
  makefile <- safe_path(system.file('run.make', package = 'makeconf'))
  args <- paste(args, collapse = " ")
  vars <- c(
    paste0('R_MAKECONF=', makeconf_path()),
    paste0('PROG=', cmd),
    paste0('ARGS=', args))
  sys::exec_internal('make', c('-f', makefile, vars), error = FALSE)
}

makeconf_path <- function(){
  conf_name <- paste0(Sys.getenv('R_ARCH'), '/makeconf')
  safe_path(file.path(R.home('etc'), conf_name))
}

safe_path <- function(x){
  x <- normalizePath(x, mustWork = TRUE)
  if(.Platform$OS.type == "windows"){
    x <- utils::shortPathName(x)
  }
  return(x)
}
