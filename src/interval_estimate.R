#interval estimate

interval_estimate <- function(merged_input){
  merged_input %>% group_by(ID,fecha,sensor) %>%
    mutate(interval_estimate = tiempo-lag(tiempo))
}