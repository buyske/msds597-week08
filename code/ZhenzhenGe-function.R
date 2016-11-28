clean_who_number <- function(x){
  ## Change formating of numbers from xxx xxx to xxxxxx
  library(stringr)
  library(dplyr)
  x <- x[!is.na(x)]
  clean_triplet_pair <- 
    function(x) str_replace(x, "([0-9]{1,3}) ([0-9]{3})", "\\1\\2")
  number_of_iterations <- max(map_int(x, ~ str_count(., " ")))
  for (i in 1:number_of_iterations){
    x <- clean_triplet_pair(x)
  }
  c1 <- data.frame(str_extract(x, "^[0-9\\.0-9]*"))
  names(c1) <- "Data"
  c1
}
extract_bounds <- function(x){
  ## Extract string in the bracket and separate
  library(stringr)
  library(dplyr)
  x <- x[!is.na(x)]
  x <- str_extract(x,"\\[.*?\\]")
  x <- gsub("\\[|\\]","", x)
  x <- data.frame(x,stringsAsFactors = FALSE)
  c2 <- separate(x, x, c("Upper_bound","Lower_bound"), sep = "-")
  c2
}

fix_who_column <- function(x){
  ## Input 1 column of data and output 3 cleaned columns
  cbind(clean_who_number(x),extract_bounds(x))
}