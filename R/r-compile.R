#' Compile source files
#'
#' Compile C / C++ / Fortran source files using the compiler configured
#' by R Makeconf.
#'
#' This program literally calls `make yourfile.o -f /path/to/R/Makeconf`.
#' This is exacltly what R does when building packages and hence the best
#' way to test if your compiler is working.
#'
#' @export
#' @name r-compile
#' @rdname r-compile
#' @param src path to source file
#' @param obj optional name of output file
#' @examples # Test the CXX compiler
#' testprog <- '#include <iostream>\nint main() {std::cout << "Hello World!";}'
#' writeLines(testprog, con = 'test.cc')
#' r_compile('test.cc')
r_compile <- function(src, obj = NULL){
  if(!length(obj))
    obj <- sub('\\.(c|cc|cpp|f|f90|f95)$', '.o', src)
  status <- sys::exec_wait('make', c(obj, '-f', makeconf_path()))
  if(!identical(status, 0L))
    stop("Failed to compile object(s): ", paste(obj, collapse = ","), call. = FALSE)
  return(obj)
}

call_with_make <- function(cmd = '$(CC)', args = '--version'){
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
