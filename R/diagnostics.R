#' Diagnostics Report
#'
#' Print some diagnostics about your compiler environment. These are also
#' shown when the `maketools` package is attached.
#'
#' @export
#' @family maketools
#' @rdname diagnostics
#' @name diagnostics
maketools_diagnostics <- function(){
  if(is_windows()){
    gcc_required <- Sys.getenv('R_COMPILED_BY', "???")
    packageStartupMessage(sprintf("This R for Windows was compiled with %s.", gcc_required))
  }
  minfo <- make_info()
  if(minfo$available == FALSE){
    packageStartupMessage('No "make" on the PATH. R will not be able to compile code.')
    return()
  }
  mversion <- ifelse(is_string(minfo$version), minfo$version, "make")
  packageStartupMessage(sprintf("Found %s in %s", mversion, minfo$path))

  pcinfo <- pc_info()
  if(pcinfo$available){
    packageStartupMessage(sprintf("Found pkg-config %s in %s", pcinfo$version, pcinfo$path))
  } else if(!is_windows()) {
    packageStartupMessage(sprintf("No pkg-config found on the path."))
  }

  ccinfo <- cc_info()
  if(ccinfo$available){
    version <- strip_banner(ccinfo$version)
    packageStartupMessage(sprintf("Using compiler %s (%s)", ccinfo$path, version))
  } else {
    packageStartupMessage(sprintf("Compiler %s not available. Cannot compile code.", ccinfo$path))
  }
}

strip_banner <- function(str){
  sub("\\s*\\(.*\\)", "", str)
}

is_string <- function(str){
  is.character(str) && length(str) && nchar(str)
}
