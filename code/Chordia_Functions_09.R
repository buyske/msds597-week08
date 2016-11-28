





xmart<-read.csv("./data/xmart.csv",stringsAsFactors = FALSE)


names(xmart)[3]<-"Maternal_Mortality"


clean_who_number_spaces<-function(x){
  
  clean_triplet_pair<-function(x)str_replace(x,"[ ]","")
  number_of_iterations <- max(map_int(x, ~ str_count(., " ")))
  for (i in 1:number_of_iterations){
    x <- clean_triplet_pair(x)
  }
  x
  
}

xmart$Maternal_Mortality<-clean_who_number_spaces(xmart$Maternal_Mortality)


clean_who_upper_bound<-function(x){
  
  clean_triplet_pair<-function(x)str_extract(x,"\\[[0-9.]{1,4}") %>% str_replace("\\[","")
  number_of_iterations <- max(map_int(x, ~ str_count(., " ")))
  for (i in 1:number_of_iterations){
    y <- clean_triplet_pair(x)
  }
  y
  
}



clean_who_lower_bound<-function(x){
  
  clean_triplet_pair<-function(x)str_extract(xmart$Maternal_Mortality,"-[0-9.]{1,4}") %>% str_replace("-","")
  number_of_iterations <- max(map_int(x, ~ str_count(., " ")))
  for (i in 1:number_of_iterations){
    y <- clean_triplet_pair(x)
  }
  y
  
}


fix_who_column<-function(x,y,a,b){
  y<-clean_who_number_spaces(x)
  a<-clean_who_upper_bound(x)
  b<-clean_who_lower_bound(x)
  df<-cbind(y,a,b)
  return(df)
}

a<-fix_who_column(x=xmart$Maternal_Mortality,a=xmart$Upper_Bound,b=xmart$Lower_Bound,y=xmart$Maternal_Mortality)
