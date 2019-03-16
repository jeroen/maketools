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
    packageStartupMessage(sprintf("Using compiler %s - %s", ccinfo$path, ccinfo$version))
  } else {
    packageStartupMessage(sprintf("Compiler %s not available. Cannot compile code.", ccinfo$path))
  }
}

find_rtools <- function(){
  if(!is_windows()){
    stop("Rtools is only needed on Windows")
  }
  rtools32 <- read_registery("SOFTWARE\\R-core\\Rtools", view = "64-bit")
  rtools64 <- read_registery("SOFTWARE\\R-core\\Rtools", view = "32-bit")
  list(rtools64, rtools32)
}

read_registery <- function(key, view){
  tryCatch(utils::readRegistry(key, hive = 'HCU', view = view, maxdepth = 2), error = function(e){
    tryCatch(utils::readRegistry(key, hive = 'HLM', view = view, maxdepth = 2), error = function(e){
      NULL
    })
  })
}
