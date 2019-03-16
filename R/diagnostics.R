#' Diagnostics Report
#'
#' Print some diagnostics about your compiler environment.
#'
#' @export
#' @rdname diagnostics
print_diagnostics <- function(){
  if(is_windows()){
    gcc_required <- Sys.getenv('R_COMPILED_BY', "???")
    packageStartupMessage(sprintf("This R for Windows was compiled with %s", gcc_required))
  }
  minfo <- make_info()
  if(minfo$available == FALSE){
    packageStartupMessage('No "make" on the PATH. R will not be able to compile code')
    return()
  }
  packageStartupMessage(sprintf("Found %s in %s", minfo$version, minfo$path))
  ccinfo <- cc_info()
  if(ccinfo$available){
    packageStartupMessage(sprintf("Working compiler available: %s - %s", ccinfo$path, ccinfo$version))
  } else {
    packageStartupMessage(sprintf("Compiler %s not available. Cannot compile code.", ccinfo$path))
  }
}
