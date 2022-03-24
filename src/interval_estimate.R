#interval estimate

interval_estimate <- function(merged_input){
  merged_input %>% group_by(ID,n_sesion,sensor) %>%
    mutate(interval_estimate = tiempo-lag(tiempo))
}
