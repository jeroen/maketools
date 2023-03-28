#' Find or Install Rtools
#'
#' Tools to test if a suitable version of Rtools is available, or help
#' the user to set this up.
#'
#' Unlike most operating systems, Windows does not include a native compiler.
#' Hence in order to build R packages with compiled C/C++/Fortran code on
#' Windows, you need to install our custom toolchain bundle called Rtools.
#'
#' There are currently 2 versions of Rtools available:
#'  - [rtools40](https://cran.r-project.org/bin/windows/Rtools/): required
#'  for compiling packages on R-4.0 and newer
#'  - [rtools35](https://cran.r-project.org/bin/windows/Rtools/history.html):
#'  required for compiling packages on R-3.6 and older
#'
#' The function [rtools_find] shows information about a suitable version of
#' Rtools installed on your system. If needed, it automatically adds `make`
#' to the PATH of the current session. If `rtools_find()` returns a list
#' (containing toolchain information), this means everything is ready to
#' install packages from source using [install.packages] and others. It
#' returns `NULL` if no suitable version is found on the system.
#'
#' The [rtools_install] function automatically downloads and installs the
#' appropriate version of Rtools for your current version of R.
#' @export
#' @family maketools
#' @name rtools
#' @rdname rtools
#' @importFrom utils download.file head tail askYesNo
rtools_find <- function(){
  info <- rtools_get_compatible()
  minfo <- make_info()
  if(!isTRUE(minfo$available && grepl("GNU", minfo$version))){
    if(length(info$PATH)){
      message(sprintf("Adding %s to the PATH", info$PATH))
      Sys.setenv(PATH = paste0(info$PATH, ';', Sys.getenv('PATH')))
    } else {
      return(NULL)
    }
  }
  ccinfo <- cc_info()
  if(!isTRUE(ccinfo$available) && length(info$BINPREF)){
    Sys.setenv(BINPREF = info$BINPREF)
    ccinfo <- cc_info()
    if(isTRUE(ccinfo$available)){
      message(sprintf("Setting BINPREF to %s (maketools)", info$BINPREF))
    } else {
      Sys.unsetenv('BINPREF')
    }
  }
  return(info)
}

rtools_get_compatible <- function(){
  info <- rtools_registry_info()
  if(isTRUE(info$compatible)) {
    return(info)
  }
}


#' @export
#' @rdname rtools
rtools_registry_info <- function(){
  assert_windows()
  x <- read_registery("SOFTWARE\\R-core\\Rtools", view = "64-bit")
  if(!length(x))
    return(NULL)
  version <- as.numeric_version(x[['Current Version']])
  install <- x[['InstallPath']]
  bindir <- paste0(install, '\\usr\\bin')
  binpref <- paste0(install, ifelse(version >= 4.2, '\\x86_64-w64-mingw32.static.posix\\bin', '\\mingw$(WIN)\\bin\\'))
  binpref <- normalizePath(binpref, winslash = '/', mustWork = FALSE)
  cc <- paste0(install, ifelse(version >= 4.2, '\\x86_64-w64-mingw32.static.posix\\bin\\gcc', '\\mingw64\\bin\\gcc'))
  gcc_version <- rtools_cc_version(cc)
  if(is_string(gcc_version)){
    available <- TRUE
    api <- gcc_api(gcc_version)
    compatible <- gcc_api(Sys.getenv('R_COMPILED_BY')) == api
  } else {
    available <- FALSE
    api <- NA
    compatible <- NA
  }
  list(
    version = version,
    compiler = gcc_version,
    api = api,
    PATH = bindir,
    BINPREF = binpref,
    available = available,
    compatible = compatible
  )
}

#' @export
#' @rdname rtools
#' @param silent performs automatic unattended installation with all
#' the default options. If set to `FALSE` the user has to click through
#' the usual installation wizard.
rtools_install <- function(silent = TRUE){
  assert_windows()
  info <- rtools_find()
  if(isTRUE(info$available)) {
    message(sprintf("Rtools %s with %s already installed: %s", info$version, info$compiler, info$PATH))
    return(invisible(info))
  }
  need_gcc <- Sys.getenv('R_COMPILED_BY')
  if(grepl('4.9.3', need_gcc)){
    message('Need GCC 4.9.3... downloading rtools35...')
    url <- 'https://cloud.r-project.org/bin/windows/Rtools/Rtools35.exe'
  } else if(grepl('8\\.\\d\\.\\d', need_gcc)){
    message('Need GCC 8... downloading rtools40...')
    url <- 'https://cran.r-project.org/bin/windows/Rtools/rtools40-x86_64.exe'
  } else {
    stop("Unsupported version of GCC: ", need_gcc)
  }
  installer <- file.path(tempdir(), basename(url))
  on.exit(unlink(installer))
  download.file(url, installer, mode = 'wb')
  args <- if(isTRUE(silent)){
    c('/SILENT', '-Wait')
  }

  # Wait but don't kill the installer when user interrupts
  pid <- sys::exec_background(installer, as.character(args))
  message("Starting installer in separate window, please wait...")
  utils::flush.console()
  if(identical(sys::exec_status(pid), 0L)) message("Success!")
  invisible(rtools_find())
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
    strip_banner(as_text(out$stdout)[1])
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

gcc_api <- function(str){
  str <- sub("gcc", "", str)
  str <- sub("\\s+", "", str)
  api <- as.numeric_version(str)
  if(api < 5){
    as.numeric_version(substring(api, 1,3))
  } else {
    as.numeric_version(gsub("\\..*", "", str))
  }
}
