care_package <- function(x){
  library(stringr)
  library(dplyr)
  }
  
clean_who_number_spaces <- 
  function(x) {
    str_replace_all(x, "([0-9]{1,3}) ([0-9]{3})", paste0("\\1", "", "\\2"))
    str_replace_all(x," ","")
  }

avg_get <-
  function(x) {
    test <- str_extract_all(x,"^[0-9]+\\.?[0-9]*\\[")
    str_replace_all(test,"\\[","")
  }

lower_get <-
  function(x) {
    test <- str_extract_all(x,"\\[[0-9]+\\.?[0-9]*")
    str_replace_all(test,"\\[","")
  }
  
upper_get <-
  function(x) {
    test <- str_extract_all(x,"[0-9]+\\.?[0-9]*\\]$")
    str_replace_all(test,"\\]","")
  }

fix_who_column <- 
  function(x) {
    care_package(x)
    prep <- clean_who_number_spaces(x)
    avg <- as.numeric(avg_get(prep))
    lower_bound <- as.numeric(lower_get(prep))
    upper_bound <- as.numeric(upper_get(prep))
    data.frame(avg,lower_bound,upper_bound)
  }