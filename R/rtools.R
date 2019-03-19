#' Setup Rtools
#'
#' Windows does not ship with a native compiler toolchain. In order to build
#' R packages with compiled code, we need the R for Windows toolchain bundle
#' called [Rtools](https://cran.r-project.org/bin/windows/Rtools/). Below are
#' utilities to check if and where Rtools is installed, and to set the proper
#' `PATH` and `BINPREF` variables to make it work.
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
      rtools = version,
      compiler = gcc_version,
      api = api,
      PATH = bindir,
      BINPREF = binpref,
      available = available,
      compatible = compatible
    )
  })
  Filter(length, installs)
}


#' @export
#' @rdname rtools
rtools_setup <- function(){
  assert_windows()
  info <- rtools_find_gcc(Sys.getenv('R_COMPILED_BY'))
  if(!isTRUE(info$available)){
    if(interactive() && isTRUE(askYesNo('Rtools not found. Would you like to install it now?'))){
      rtools_install()
      info <- rtools_find_gcc(Sys.getenv('R_COMPILED_BY'))
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
#' @param silent perform automatic unattended installation (answer YES
#' to all questions)
rtools_install <- function(silent = FALSE){
  assert_windows()
  need_gcc <- Sys.getenv('R_COMPILED_BY')
  if(!is_string(need_gcc)){
    stop("Did not find R_COMPILED_BY variable")
  }
  info <- rtools_find_gcc(need_gcc)
  if(isTRUE(info$available)) {
    message(sprintf("Rtools %s with %s already installed: %s", info$rtools, need_gcc, info$PATH))
    return(invisible())
  }

  if(grepl('4.9.3', need_gcc)){
    message('Need GCC 4.9.3... downloading rtools35...')
    url <- 'https://cloud.r-project.org/bin/windows/Rtools/Rtools35.exe'
  } else if(grepl('8\\.\\d\\.\\d', need_gcc)){
    message('Need GCC 8... downloading rtools40...')
    url <- 'https://cran.r-project.org/bin/windows/testing/rtools40-x86_64.exe'
  } else {
    stop("Unsupported version of GCC: ", need_gcc)
  }
  installer <- file.path(tempdir(), basename(url))
  on.exit(unlink(installer))
  download.file(url, installer, mode = 'wb')
  args <- if(isTRUE(silent)){
    c('/VERYSILENT', '-Wait')
  }

  # Wait but don't kill the installer when user interrupts
  pid <- sys::exec_background(installer, as.character(args))
  if(identical(sys::exec_status(pid), 0L)) message("Success!")
}


rtools_find_gcc <- function(need_gcc){
  info <- rtools_info()
  for(x in info){
    if(isTRUE(x$compatible)) {
      return(x)
    }
  }
  return(NULL)
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
