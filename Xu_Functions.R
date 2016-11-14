clean_who_number_spaces <- function(x){
    library(stringr)
    library(dplyr)
    x <- str_replace_all(x, " ", "")
    x
  }

upper_bound <- function(x){
  library(stringr)
  library(dplyr)
  x<-str_extract_all(x, "\\[[0-9]+")
  x <- str_replace(x , "\\[" , "")
  x<- as.numeric(x)
}

lower_bound <- function(x){
  library(stringr)
  library(dplyr)
x<-str_extract_all(x, "[0-9]+\\]")
x<- str_replace(x , "\\]" , "")
x<- as.numeric(x)
 }


rate <- function(x){
  library(stringr)
  library(dplyr)
x<-str_extract(x, "[0-9 .]+\\[")
x<-str_replace_all(x,"[ \\[]", "") 
x<-as.numeric(x)
  }


fix_who_column <- function(x){
   x <- clean_who_number_spaces(x)
   x <- data.frame(upper_bound(x), lower_bound(x), rate(x)) 
   x
  }