#' R CMD Config
#'
#' Cross-platform wrappers for `R CMD config` to lookup the availability
#' of the compiler.
#'
#' @export
#' @name r_config
#' @rdname r_config
#' @examples # This runs 'R CMD CONFIG CXX11'
#' r_cmd_config("CXX11")
#'
#' # Show your C++11 compiler:
#' r_test_cxx11()
r_test_cc <- function(){
  make_test('CC')
}

#' @export
#' @rdname r_config
r_test_cxx <- function(){
  make_test('CXX')
}

#' @export
#' @rdname r_config
r_test_cxx11 <- function(){
  make_test('CXX11')
}

#' @export
#' @rdname r_config
r_test_fc <- function(){
  make_test('FC')
}
