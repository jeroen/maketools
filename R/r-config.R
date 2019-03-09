#' R CMD Config
#'
#' Cross-platform wrappers for `R CMD config` to lookup the availability
#' of the compiler.
#'
#' @export
#' @name r_cmd_config
#' @rdname r_cmd_config
r_has_cc <- function(){
  r_has('$(CC)')
}

#' @export
#' @rdname r_cmd_config
r_has_cxx <- function(){
  r_has('$(CXX)')
}

#' @export
#' @rdname r_cmd_config
r_has_cxx11 <- function(){
  r_has('$(CXX11)')
}

#' @export
#' @rdname r_cmd_config
r_has_fc <- function(){
  r_has('$(FC)')
}

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
r_has <-function(VAR){
  #VAR <- r_cmd_config(VAR)
  call_with_make(VAR, '--version') == 0
}
