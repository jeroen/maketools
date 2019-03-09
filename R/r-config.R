#' R CMD Config
#'
#' Cross-platform wrappers for `R CMD config` to lookup the availability
#' of the compiler.
#'
#' @export
#' @name r_config
#' @rdname r_config
#' @examples # This runs 'R CMD CONFIG CXX'
#' r_cmd_config("CXX")
#'
#' # Show default C++ compiler:
#' r_test_cxx()
r_test_cc <- function(){
  r_make_test('CC', 'CFLAGS')
}

#' @export
#' @rdname r_config
r_test_cxx <- function(){
  r_make_test('CXX', 'CXXFLAGS')
}

#' @export
#' @rdname r_config
r_test_cxx11 <- function(){
  r_make_test('CXX11', 'CXX11FLAGS', 'CXX11STD')
}

#' @export
#' @rdname r_config
r_test_cxx14 <- function(){
  r_make_test('CXX14', 'CXX14FLAGS', 'CXX14STD')
}

#' @export
#' @rdname r_config
r_test_cxx17 <- function(){
  r_make_test('CXX17', 'CXX17FLAGS', 'CXX17STD')
}

#' @export
#' @rdname r_config
r_test_fc <- function(){
  r_make_test('FC', 'FCFLAGS')
}

r_make_test <-function(VAR, FLAGS = NULL, STD = NULL, args = '--version'){
  PATH <- r_cmd_config(VAR)
  if(length(FLAGS))
    FLAGS <- r_cmd_config(FLAGS)
  if(length(STD))
    STD <- r_cmd_config(STD)
  info <- r_make_call(PATH, args)
  version <- as_text(info$stdout)
  list(
    path = PATH,
    flags = FLAGS,
    std = STD,
    available = info$status == 0,
    version = version[1]
  )
}
