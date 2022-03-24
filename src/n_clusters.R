x <- c(1,1,1,2,2,1,1,1,1,2,2,1,1)
library(tidyverse)

data<- read_csv("../test/files/merged_example.csv")

counter <- function(x){
  counter = 0
  out = c(0)
  for (i in 1:length(x)){
    if (i > 1){
      if(x[i] != x[i-1]){
        counter = counter + 1
        out = append(out, counter)
      }
      else{
        out = append(out, counter)
      }    
    }
  }
  return(out)
} 


n_clusters <- function(merged_input){
  merged_input %>% mutate(tt=as.numeric(as.factor(cluster_bool))) %>% View
}

n_clusters(data)


lag(x[4]) != x[4]

counter = 0
out = c()
for (i in 1:length(x)){
  if (i > 1){
    if(x[i] != x[i-1]){
      counter = counter + 1
      out = append(out, counter)
    }
    else{
      out = append(out, counter)
    }    
  }
}
out

