transf_cont <- function(x, no_valid_labs, min = "auto", max = "auto", digits = 0) {
  
  if (identical(min, "auto") && identical(max, "auto")) {
    nejmz <- names(labelled::val_labels(x))[1:no_valid_labs]
    nejmz.brojke <- str_extract_all(nejmz, "[0-9]+")
    mini <- lapply(nejmz.brojke, function(x) x[1]) %>% unlist %>% as.numeric()
    maxi <- lapply(nejmz.brojke, function(x) x[2]) %>% unlist %>% as.numeric()
  } else {
    if (length(min) == length(max)) {
      mini <- min
      maxi <- max
      warning("auto not used - check min and max carefully", call. = FALSE)  
    } else stop("min-max lengths unequal")
  }
  
  set.seed(666)
  x <- as.numeric(x)                         # 0. input
  
  for (i in seq_along(1:no_valid_labs)) {    # 2. sequence
    
    x[x == i] <- runif(length(x[x == i]), min = mini[i], max = maxi[i])
    
  }
  
  round(x, digits = digits)                  # 3. output
  
}

