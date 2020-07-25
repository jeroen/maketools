#' Make
#'
#' Compile C / C++ / Fortran source files using the compiler configured
#' by your R `Makeconf` file.
#'
#' The `make` function literally calls `make yourfile.o -f /path/to/R/Makeconf`.
#' This is exactly what R does when building packages and hence the best
#' way to test if the compiler is working.
#'
#' @export
#' @name make
#' @rdname make
#' @param target name of output file that you want to make
#' @param makefile path to the `Makefile`. Defaults to the `Makeconf` which
#' R uses when building R packages.
#' @examples # Test the CXX compiler
#' testprog <- '#include <iostream>\nint main() {std::cout << "Hello World!";}'
#' writeLines(testprog, con = 'testprog.cc')
#' make('testprog')
#'
#' # Test and cleanup
#' system('./testprog')
#' unlink('testprog*', recursive = TRUE)
#'
make <- function(target = 'all', makefile = r_makeconf_path()){
  status <- sys::exec_wait(r_make_path(), c(target, '-f', makefile))
  if(!identical(status, 0L))
    stop("Failed to compile object(s): ", paste(target, collapse = ","), call. = FALSE)
  return(target)
}

#' @export
#' @rdname make
#' @param cmd command to invoke (may be a variable)
#' @param args additional arguments for `cmd`
#' @examples # Run a program from a make variable
#' make_call('$(CXX)', '--version')
#'
make_call <- function(cmd = '$(CC)', args = '--version'){
  testmake <- ifelse(is_solaris(), 'solaris.make', 'test.make')
  makefile <- safe_path(system.file(testmake, package = 'maketools'))
  args <- paste(args, collapse = " ")
  vars <- c(
    paste0('R_MAKECONF=', r_makeconf_path()),
    paste0('PROG=', cmd),
    paste0('ARGS=', args))
  r_exec_make(c('-f', makefile, vars))
}

#' @export
#' @rdname make
make_echo <- function(cmd = '$(CC)'){
  testmake <- ifelse(is_solaris(), 'solaris.make', 'test.make')
  makefile <- safe_path(system.file(testmake, package = 'maketools'))
  vars <- c(
    paste0('R_MAKECONF=', r_makeconf_path()),
    paste0('PROG=', cmd))
  out <- r_exec_make(c('print-value', '-f', makefile, vars))
  as_text(out$stdout)
}

#' @export
#' @rdname make
#' @examples # Where your makeconf is stored:
#' make_info()
#'
make_info <- function(){
  name <- r_make_path()
  path <- unname(Sys.which(name))
  available <- is_string(path)
  version <- if(available){
    info <- r_exec_make('--version')
    if(info$status == 0){
      as_text(info$stdout)[1]
    }
  }
  list(
    name = name,
    available = available,
    path = path,
    version = version,
    makeconf = r_makeconf_path()
  )
}

assert_make_available <- function(){
  path <- Sys.which(r_make_path())
  if(!is_string(path)){
    stop("Did not find 'make' on the PATH.", call. = FALSE)
  }
}

r_exec_make <- function(args){
  assert_make_available()
  exec_internal(r_make_path(), args, error = FALSE)
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

is_solaris <- function(){
  Sys.info()[["sysname"]] == "SunOS"
}
