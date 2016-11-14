library(tidyverse)
library(stringr)

# function to remove spaces in maternal mortality numbers where commas should be
who_remove_white_space <- function(col) {
  col <- str_replace_all(col, "[\\s]+", "")
  return(col)
}

who_break_out_lower_bounds <- function(col){
  # grab number between - and ]
  # remove [ and - so only left with lower bound number
  # convert the data to numbers
  lower_bound <- str_extract(col, "[\\[][0-9\\.]*\\b") %>%
    str_replace("[\\[]", "") %>%
    as.numeric(.)
  return(lower_bound)
}

who_break_out_upper_bounds <- function(col){
  # grab number between - and ]
  # remove [ and - so only left with upper bound number
  # convert the data to numbers
  upper_bound <- str_extract(col, "[\\-][0-9\\.]*\\b") %>%
    str_replace("[\\]\\-]", "") %>%
    as.numeric(.)
  return(upper_bound)
}

who_break_out_mm <- function(col){
  # get rid of everything but the first number from col
  mat_mort <- str_replace(col, "[\\[][0-9\\.]*[-][0-9\\.]*[\\]]", "")
  
  # convert the data to numbers
  mat_mort <- as.numeric(mat_mort)
  return(mat_mort)
}

fix_who_column <- function(col){
  # remove all white space from column
  col <- who_remove_white_space(col)
  
  # extract lower and upper bounds
  lower_bound <- who_break_out_lower_bounds(col)
  upper_bound <- who_break_out_upper_bounds(col)
  
  # extract maternal mortality
  actual      <- who_break_out_mm(col)
  
  # create a three column tibble and return it
  mm_data <- tibble(lower_bound, upper_bound, actual)
  return(mm_data)
}

# read in and tidy up the maternal mortality data
# mm <- read_csv("../data/xmart.csv")
# names(mm) <- c("country", "year", "maternal_mortality", "skilled_health_percentage")

# mm_dat <- fix_who_column(mm$maternal_mortality)

# mm$maternal_mortality <- NULL
# mm$maternal_mortality <- mm_dat$actual
# mm$mm_lower_bound <- mm_dat$lower_bound
# mm$mm_upper_bound <- mm_dat$upper_bound

# mm
