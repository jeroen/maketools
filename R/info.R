#' R CMD Config
#'
#' Cross-platform wrappers for `R CMD config` to lookup the availability
#' of the compiler.
#'
#' @export
#' @family maketools
#' @name r_config
#' @rdname r_config
#' @examples # This runs 'R CMD CONFIG CXX'
#' r_cmd_config("CXX")
#'
#' # Show C++ config:
#' cxx_info()
cc_info <- function(){
  make_get_info('CC', 'CFLAGS')
}

#' @export
#' @rdname r_config
cxx_info <- function(){
  make_get_info('CXX', 'CXXFLAGS')
}

#' @export
#' @rdname r_config
cxx11_info <- function(){
  make_get_info('CXX11', 'CXX11FLAGS', 'CXX11STD')
}

#' @export
#' @rdname r_config
cxx14_info <- function(){
  make_get_info('CXX14', 'CXX14FLAGS', 'CXX14STD')
}

#' @export
#' @rdname r_config
cxx17_info <- function(){
  make_get_info('CXX17', 'CXX17FLAGS', 'CXX17STD')
}

#' @export
#' @rdname r_config
fc_info <- function(){
  make_get_info('FC', 'FCFLAGS')
}

#' @export
#' @rdname r_config
#' @importFrom sys as_text exec_internal
#' @param VAR value passed to `R CMD config` such as `CXX` or `FC`
r_cmd_config <- function(VAR = '--all'){
  assert_make_available()
  R <- file.path(R.home('bin'), 'R')
  outcon <- rawConnection(raw(0), "r+")
  on.exit(close(outcon), add = TRUE)
  out <- exec_internal(R, c("CMD", "config", VAR), error = FALSE)
  if(out$status == 0){
    as_text(out$stdout)
  } else {
    stop(as_text(c(out$stdout, out$stderr)), call. = FALSE)
  }
}

make_get_info <-function(VAR, FLAGS = NULL, STD = NULL, args = '--version'){
  PATH <- r_cmd_config(VAR)
  if(length(FLAGS))
    FLAGS <- r_cmd_config(FLAGS)
  if(length(STD))
    STD <- r_cmd_config(STD)
  info <- make_call(PATH, args)
  version <- as_text(info$stdout)
  list(
    path = PATH,
    flags = FLAGS,
    std = STD,
    available = info$status == 0,
    version = version[1]
  )
}
