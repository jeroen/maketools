# Load tibble (if available) for pretty printing
.onAttach <- function(lib, pkg){
  print_diagnostics()
  if(interactive() && is.null(.getNamespace('tibble'))){
    tryCatch({
      getNamespace('tibble')
    }, error = function(e){})
  }
}

.onLoad <- function(lib, pkg){
  if(is_windows()){
    if(!length(rtools_setup())){
      packageStartupMessage("No suitable Rtools installation found.
Run maketools::rtools_install() or visit: https://cran.r-project.org/bin/windows/Rtools/")
    }
  }
}
