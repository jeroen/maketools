#' Find or Setup Rtools
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
#' The function [rtools_info] lists the Rtools versions that are installed
#' on your system (without touching any configurations).
#'
#' The [rtools_setup] function interactively guides the user through setting
#' up and/or configuring Rtools. If rtools is not already installed, it will
#' prompt the user to do so. If needed, it sets the PATH and other variables
#' to the correct values. After running `rtools_setup()` everything is set
#' to install packages from source using [install.packages] and others.
#'
#' @export
#' @name rtools
#' @rdname rtools
#' @importFrom utils download.file
rtools_info <- function(){
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
  })
  # In theory, rtools40 can be 32-bit Windows host but this is very rare
  # and probably the above doesn't work in that case anyway.
  structure(installs, names = c("rtools4", "rtools3"))
}

#' @export
#' @rdname rtools
rtools_find <- function(){
  info <- rtools_info()
  for(x in info){
    if(isTRUE(x$compatible)) {
      return(x)
    }
  }
  return(NULL)
}


#' @export
#' @rdname rtools
#' @importFrom utils head tail askYesNo
rtools_setup <- function(){
  assert_windows()
  info <- rtools_find()
  if(!isTRUE(info$available)){
    if(interactive() && isTRUE(askYesNo('Rtools not found. Would you like to install it now?'))){
      rtools_install()
      info <- rtools_find()
    } else {
      stop("Rtools not found. Please run: rtools_install()")
    }
  }
  rtools_make <- Sys.which(file.path(info$PATH, 'make'))
  if(unname(Sys.which('make')) == normalizePath(rtools_make)){
    message(sprintf("Correct make already on the path: %s", rtools_make))
  } else {
    message(sprintf("Adding %s to the PATH", info$PATH))
    Sys.setenv(PATH = paste0(info$PATH, ';', Sys.getenv('PATH')))
  }
  ccinfo <- cc_info()
  if(!isTRUE(ccinfo$available)){
    Sys.setenv(BINPREF = info$BINPREF)
    ccinfo <- cc_info()
    if(isTRUE(ccinfo$available)){
      message("Successfully set BINPREF variable")
    } else {
      Sys.unsetenv('BINPREF')
    }
  }
  print_diagnostics()
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
    message(sprintf("Rtools %s with %s already installed: %s", info$rtools, info$compiler, info$PATH))
    return(invisible())
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
