# klonirajmo varijable #

# napiši funkciju koja primi data frejm i klonira ga sa sufiksom

clone_R <- function(x.df, sufiks = "_R") {
  
  colnames(x.df) <- paste0(colnames(x.df), sufiks)
  
  x.df
  
}

set_var_labs <- function (.data, ...) {
  
  if (is.list(...)) values <- unlist(list(...)) else values <- list(...)
  
  if (!all(names(values) %in% names(.data))) 
    stop("some variables not found in .data")
  for (v in names(values)) var_label(.data[[v]]) <- values[[v]]
  .data
}

clone_R(mtcars)
