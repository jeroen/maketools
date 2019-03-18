#' Find Rtools
#'
#' Lookup and test installations of Rtools in Windows registry.
#'
#' @export
find_rtools <- function(){
  if(!is_windows()){
    stop("Rtools is only needed on Windows")
  }
  rtools64 <- read_registery("SOFTWARE\\R-core\\Rtools", view = "64-bit")
  rtools32 <- read_registery("SOFTWARE\\R-core\\Rtools", view = "32-bit")
  installs <- lapply(list(rtools32, rtools64), function(x){
    version <- as.numeric_version(x[['Current Version']])
    install <- x[['InstallPath']]
    bindir <- paste0(install, ifelse(version >= 4, '\\usr\\bin', '\\bin'))
    binpref <- paste0(install, ifelse(version >= 4, '\\mingw$(WIN)\\bin\\', '\\mingw_$(WIN)\\bin\\'))
    binpref <- normalizePath(binpref, winslash = '/', mustWork = FALSE)
    cc <- paste0(install, ifelse(version >= 4, '\\mingw32\\bin\\gcc', '\\mingw_32\\bin\\gcc'))
    gcc_version <- rtools_cc_version(cc)
    list(
      rtools = version,
      compiler = gcc_version,
      PATH = bindir,
      BINPREF = binpref,
      available = length(gcc_version) && nchar(gcc_version)
    )
  })
  installs
}

read_registery <- function(key, view){
  tryCatch(utils::readRegistry(key, hive = 'HCU', view = view, maxdepth = 2), error = function(e){
    tryCatch(utils::readRegistry(key, hive = 'HLM', view = view, maxdepth = 2), error = function(e){
      NULL
    })
  })
}

rtools_cc_version <- function(cc){
  out <- sys::exec_internal(cc, '--version', error = FALSE)
  if(out$status == 0){
    sub(" (.*) ", " ", as_text(out$stdout)[1])
  } else{
    warning(as_text(c(out$stdout, out$stderr)), immediate. = TRUE)
    NULL
  }
}
