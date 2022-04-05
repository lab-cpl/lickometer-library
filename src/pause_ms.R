#necesito el ID único de cada cluster para poder separar las pausas

pause_ms <- function(merged_data) {
  diff <- merged_data %>%
    subset(select=c(ID, sensor, n_sesion, cluster_bool, bins,tiempo))%>%
    group_by(ID, sensor, n_sesion, cluster_bool, bins) %>%
    mutate(t_min = min(tiempo),
           t_max = max(tiempo)) %>%
    mutate(valid_pause = ifelse(cluster_bool == "out_cluster", NA, "")) %>%
    subset(select = -tiempo) %>%
    unique() %>%
    drop_na() %>%
    ungroup() %>%
    subset(select = -valid_pause) %>%
    group_by(ID, sensor, n_sesion, cluster_bool) %>%
    mutate(pause_ms = lead(t_min) - t_max) %>%
    ungroup()
  
  dplyr::left_join(merged_data, diff)
}

# merged_data <-
#   data.frame(
#     ID = c("300", "300", "300", "300", "300", "300", "300", "300", "300"),
#     sensor = c("1", "1", "1", "1", "1", "1", "1", "1", "1"),
#     n_sesion = c("5", "5", "5", "5", "5", "5", "5", "5", "5"),
#     bins = c("1", "2", "3", "3", "4", "4", "5", "5", "5"),
#     tiempo = c(100, 120, 300, 350, 400, 500, 550, 700, 800),
#     cluster_bool = c(
#       "in_cluster",
#       "out_cluster",
#       "in_cluster",
#       "in_cluster",
#       "out_cluster",
#       "out_cluster",
#       "in_cluster",
#       "in_cluster",
#       "in_cluster"
#     )
#   )
# 

merged_data <- data_final 
view(pause_ms(merged_data))