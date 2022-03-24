cluster_size <- function(merged_input){
  merged_input %>% group_by(ID,fecha,n_licometro) %>%
    mutate(cluster_size = sum(bins)
  )
}