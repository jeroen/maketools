#' Find Rtools
#'
#' Lookup and test installations of Rtools in Windows registry.
#'
#' @export
#' @rdname rtools
find_rtools <- function(){
  assert_windows()
  installs <- lapply(c("64-bit", "32-bit"), function(view){
    x <- read_registery("SOFTWARE\\R-core\\Rtools", view = view)
    if(!length(x))
      return(NULL)
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
  Filter(length, installs)
}

#' @export
#' @rdname rtools
#' @param silent perform fully automatic unattended installation
install_rtools <- function(silent = FALSE){
  assert_windows()
  need_gcc <- Sys.getenv('R_COMPILED_BY')
  if(!nchar(need_gcc)){
    stop("Did not find R_COMPILED_BY variable")
  }
  info <- try(find_rtools())
  if(grepl('4.9.3', need_gcc)){
    message('Need GCC 4.9.3... downloading rtools35...')
    url <- 'https://cloud.r-project.org/bin/windows/Rtools/Rtools35.exe'
  } else if(grepl('8\\.\\d\\.\\d', need_gcc)){
    message('Need GCC 8... downloading rtools40...')
    url <- 'https://cran.r-project.org/bin/windows/testing/rtools40-x86_64.exe'
  } else {
    stop("Unsupported version of GCC: ", need_gcc)
  }
  installer <- basename(url)
  on.exit(unlink(installer))
  download.file(url, installer, mode = 'wb')
  args <- if(isTRUE(silent)){
    c('/VERYSILENT', '-NoNewWindow', '-Wait')
  }
  status <- sys::exec_wait(installer, as.character(args))
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

assert_windows <- function(){
  if(!is_windows()){
    stop("Rtools is only needed on Windows")
  }
}
