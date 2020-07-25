# Load tibble (if available) for pretty printing
.onAttach <- function(lib, pkg){
  maketools_diagnostics()
  if(interactive() && is.null(.getNamespace('tibble'))){
    tryCatch({
      getNamespace('tibble')
    }, error = function(e){})
  }
}

.onLoad <- function(lib, pkg){
  if(is_windows()){
    rtools_find()
  }
}
