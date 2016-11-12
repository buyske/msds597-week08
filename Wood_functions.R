
library(tidyverse)
library(stringr)

mat_mor <- read_csv("http://apps.who.int/gho/athena/data/xmart.csv?target=GHO/MDG_0000000025,MDG_0000000026&profile=crosstable&filter=COUNTRY:*;YEAR:*&x-sideaxis=COUNTRY;YEAR&x-topaxis=GHO")
colnames(mat_mor)[3:4] <- c("mortality.rate", "attended.births")

remove_spaces <- function(x) {
  x <-as.data.frame(x, stringsAsFactors = FALSE)
num_row <- nrow(x)
  for (i in 1:num_row){
    x[i,] <-str_replace_all(x[i,], " ", "")
  }
x
}


separate_bounds <- function(x) {
  x <- as.data.frame(x)
  x <- separate(x, x, into = c("x", "bounds"), sep = "\\[",fill = "right")
  x$bounds <- str_replace_all(x$bounds, "\\]", "")
  x <- separate(x, bounds, into = c("lower_bound", "upper_bound"), sep = "\\-",fill = "right")
  x
}


fix_who_column <- function(x) {
y <- remove_spaces(x)
z <- separate_bounds(y)

z 
}

mat_mor_new <- fix_who_column(mat_mor$mortality.rate)
glimpse(mat_mor_new)
