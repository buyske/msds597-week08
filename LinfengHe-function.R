clean_who_number_spaces <- function(x, sep = ""){
  ## Change formating of numbers from xxx xxx to xxxxxx
  library(stringr)
  library(dplyr)
  clean_triplet_pair <- 
    function(x) str_replace(x, "([0-9]{1,3}) ([0-9]{3})", paste0("\\1\\2"))
  number_of_iterations <- max(map_int(x, ~ str_count(., " ")), na.rm = TRUE)
  for (i in 1:number_of_iterations){
    x <- clean_triplet_pair(x)
  }
  x
}

who_pull_middle <- function(x){
  library(stringr)
  library(dplyr)
  x <- str_extract(x, "[0-9]+") %>%
    as.numeric()
  return(x)
}

who_pull_lower_bounds <- function(x){
  library(stringr)
  library(dplyr)
  x <- x %>%
    str_extract(., "\\[ [0-9]+") %>%
    str_replace_all(., "\\[","") %>%
    as.numeric()
  return(x)
}

who_pull_upper_bounds <- function(x){
  library(stringr)
  library(dplyr)
  x <- x %>%
    str_extract(., "[0-9]+\\]") %>%
    str_replace_all(., "\\]", "") %>%
    as.numeric()
  return(x)
}
  
fix_who_column <- function(x){
  x <- clean_who_number_spaces(x)
  x <- cbind(who_pull_middle(x), who_pull_lower_bounds(x), who_pull_upper_bounds(x))
  return(x)
}