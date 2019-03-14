# Load tibble (if available) for pretty printing
.onAttach <- function(lib, pkg){
  if(interactive() && is.null(.getNamespace('tibble'))){
    tryCatch({
      getNamespace('tibble')
    }, error = function(e){})
  }
}
