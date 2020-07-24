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
#' @name make
#' @rdname make
#' @param target name of output file that you want to make
#' @param makefile path to the `Makefile`. Defaults to the `Makeconf` which
#' R uses when building R packages.
#' @examples # Test the CXX compiler
#' testprog <- '#include <iostream>\nint main() {std::cout << "Hello World!";}'
#' writeLines(testprog, con = 'testprog.cc')
#' make('testprog')
#' system('./testprog')
#'
#' # Cleanup
#' unlink('testprog*', recursive = TRUE)
make <- function(target = 'all', makefile = r_makeconf_path()){
  status <- sys::exec_wait(r_make_path(), c(target, '-f', makefile))
  if(!identical(status, 0L))
    stop("Failed to compile object(s): ", paste(target, collapse = ","), call. = FALSE)
  return(target)
}
