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
r_test_make <- function(){
  make <- Sys.getenv('MAKE', 'make')
}

r_test_cc <- function(){
  r_make_test('CC')
}

#' @export
#' @rdname r_config
r_test_cxx <- function(){
  r_make_test('CXX')
}

#' @export
#' @rdname r_config
r_test_cxx11 <- function(){
  r_make_test('CXX11')
}

#' @export
#' @rdname r_config
r_test_cxx14 <- function(){
  r_make_test('CXX14')
}

#' @export
#' @rdname r_config
r_test_cxx17 <- function(){
  r_make_test('CXX17')
}

#' @export
#' @rdname r_config
r_test_fc <- function(){
  r_make_test('FC')
}

r_make_test <-function(VAR, args = '--version'){
  PATH <- r_cmd_config(VAR)
  FLAGS <- r_cmd_config(paste0(VAR, 'FLAGS'))
  STD <- if(grepl("^CXX", VAR)){
    tryCatch(r_cmd_config(paste0(VAR, 'STD')), error = function(e){""})
  }
  info <- r_make_call(PATH, args)
  version <- as_text(info$stdout)
  list(
    path = PATH,
    std = STD,
    flags = FLAGS,
    available = info$status == 0,
    version = version[1]
  )
}
