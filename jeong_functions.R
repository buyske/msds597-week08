---
title: "Jeong_Week_08"
author: "Kiyoon Jeong"
date: "November 12, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(stringr)
library(dplyr)

```

##Kiyoon_Function

```{r function}

fix_who_column <- function(x){
  
clean_who_number_spaces <- function(x, sep = ""){
  clean_triplet_pair <- 
    function(x) str_replace(x, "([0-9]{1,3}) ([0-9]{3})", paste0("\\1", sep, "\\2"))
  number_of_iterations <- max(map_int(x, ~ str_count(., " ")), na.rm = TRUE)
  for (i in 1:number_of_iterations){
    x <- clean_triplet_pair(x)
  }
  x <- data.frame(x, stringsAsFactors = FALSE)
  x
}
x <- clean_who_number_spaces(x)

x$x <- as.factor(x$x)

x <- separate(x, x, into = c("mortality.rate" , "Bound") , sep = "\\[" , fill = "right")

x$Bound <- str_replace_all(x$Bound, "\\]", "")

x <- separate(x, Bound , into = c("Lower" , "Upper") , sep = " \\- " , fill = "right")

x
}

```
