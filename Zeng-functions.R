library(stringr)
library(Lahman)
library(dplyr)
library(plyr)
library(tidyverse)


clean_who_spaces <- function(x){
  library(stringr)
  library(dplyr)
  clean_column <- str_replace_all(x, " ", "")
  clean_column
}

extract_bounds <- function(x) {df<-data.frame(ldply(strsplit(x, "\\["), rbind))
df1<-data.frame(ldply(str_split(df$X2, "-"), rbind))
df2<-data.frame(cbind(df$X1, df1))
df2$X2<-str_replace_all(df2$X2, "\\]", "")
df2<-rename(df2, c("df.X1"="Rate", "X1"="lower_bound", "X2"="upper_bound"))
df2   
}

fix_who_column <- function(x) {df<-clean_who_spaces(x) %>% extract_bounds()
df
}

