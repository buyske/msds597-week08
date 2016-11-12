##Subhasree Samanta
##MSDS597 Assignment 8

##Function to remove spaces from column
clean_who_spaces <- function(x, sep = ""){
  x <- gsub(" ","",x)
}

##Remove bounds from column (keeping only average). 
##Separated from clean_who_number_spaces so that function can be used for the bound extraction as well
clean_who_remove_bounds <- function(x, sep = ""){
  x <- as.integer(gsub("\\[[ 0-9 -]+\\]","", clean_who_spaces(x)))
}

##Function to extract upper & lower bounds
extract_who_bounds <- function(x, sep = ""){
  library(stringr)
  x <- clean_who_spaces(x)
  extract_lower <-
    function(x) {
      x <- as.integer(gsub("\\[","", str_split_fixed(str_extract(x, "\\[[ 0-9 -]+\\]"), "-", 2)[,1]))
    }
  extract_upper <-
    function(x) {
      x <- as.integer(gsub("\\]","", str_split_fixed(str_extract(x, "\\[[ 0-9 -]+\\]"), "-", 2)[,2]))
    }
 x <- cbind(extract_lower(x), extract_upper(x))
}

## Last function, takes 1 col input, returns 3 col output
fix_who_column <- function(x, sep = ""){
  x <- cbind(clean_who_remove_bounds(x),extract_who_bounds(x))
  colnames(x) <- c("avg","lower_bound","upper_bound")
  x
}
