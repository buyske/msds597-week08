library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(rvest)
library(readxl)
library(viridis)
library(plotly)


# Reading the data from the database and putting it in a table "xmart"
xmart<-read.csv("E:/DataScience/DataWrangling/Assignment8/xmart.csv",stringsAsFactors = FALSE)

names(xmart)[3]<-"Maternal_Mortality"

# Calling a function to remove all the whit spaces from the column Maternal_Mortality
clean_who_number_spaces<-function(x){

  clean_triplet_pair<-function(x)x <- str_replace(x, "[[:space:]]","") %>%  str_replace("[[:space:]]+","") %>%   str_replace("[[:space:]]+","")
  number_of_iterations <- max(map_int(x, ~ str_count(., " ")))
  for (i in 1:number_of_iterations){
    x <- clean_triplet_pair(x)
  }
  x
}

# Calling a function to take all the lower bound values from the Maternal_Mortality
clean_who_lower_bound<-function(x){
  
    k <- str_trim(str_extract(x,"\\[[[[:space:]]0-9]+.[0-9]+") %>% str_replace("\\[",""))
 }


# Calling a function to take all the upper bound values from the Maternal_Mortality

clean_who_upper_bound<-function(x){
  k <- str_trim(str_extract(x,"\\-[[[:space:]]0-9]+.[0-9]+") %>% str_replace("-",""))
}




# Function  that calls all of those functions in order to take as input a column in the WHO format and output a 3 column data frame. Naming the last function "fix_who_column".

fix_who_column <- function(x){
  
  Maternal_Mortality <- clean_who_number_spaces(x)
  lower.bound <- sapply(x,FUN = clean_who_lower_bound)
  upper.bound <- sapply(x,FUN = clean_who_upper_bound)
  output <- as.data.frame(Maternal_Mortality)
  output$lower.bound <- lower.bound
  output$upper.bound <- upper.bound
  output
}

head(fix_who_column(xmart$Maternal_Mortality))


