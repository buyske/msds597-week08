clean_who_number_spaces <- function(x){
  library(stringr)
  library(dplyr)
  x <- str_replace_all(x, " ", "")
  x
}
pull_out_rate <- function(x){
  library(stringr)
  library(dplyr)
  x <- str_extract(x, "[0-9 .]+\\[") %>% str_replace_all("[ \\[]", "") 
  x
}
pull_out_lower_bound <- function(x){
  library(stringr)
  library(dplyr)
  x <- str_extract(x, "\\[[ 0-9.]+") %>% str_replace_all("[ \\[]", "") 
  x
}
pull_out_upper_bound <- function(x){
  library(stringr)
  library(dplyr)
  x <- str_extract(x, "-[ 0-9.]+") %>% str_replace_all("[ -]", "") 
  x
}
fix_who_column <- function(x){
  x <- clean_who_number_spaces(x)
  x <- data.frame(pull_out_rate(x), pull_out_lower_bound(x), pull_out_upper_bound(x)) 
  x
}
