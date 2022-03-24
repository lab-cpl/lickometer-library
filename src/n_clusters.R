library(tidyverse)

data<- read_csv("../test/files/merged_example.csv")

make_groups <- function(x){
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
	merged_input %>%
		mutate(cluster_bool = cluster_bool %>%
		       as.factor() %>%
		       as.numeric()
	       )
}

n_clusters(data) %>% select(cluster_bool) %>% max()
