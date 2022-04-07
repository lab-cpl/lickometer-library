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
		group_by(ID, sensor, n_sesion) %>%
		mutate(
			tmp_bool = as.numeric(as.factor(cluster_bool)) %>% replace_na(0),
			n_clusters = make_groups(tmp_bool)
		) %>%
		select(-tmp_bool) %>%
		ungroup() -> out
	out %>%
		group_by(ID, sensor, n_sesion, n_clusters) %>%
		summarise(cluster_size = n()) %>%
		left_join(out) %>%
		ungroup() -> outt
	return(out)
}
