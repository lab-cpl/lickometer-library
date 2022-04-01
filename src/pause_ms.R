#necesito el ID único de cada cluster para poder separar las pausas

merged_data <-
  data.frame(
    ID = c("300", "300", "300", "300", "300", "300", "300", "300", "300"),
    sensor = c("1", "1", "1", "1", "1", "1", "1", "1", "1"),
    n_sesion = c("5", "5", "5", "5", "5", "5", "5", "5", "5"),
    bins = c("1","2","3","3","4","4","5","5","5"),
    tiempo = c(100, 120, 300, 350, 400, 500, 550, 700, 800),
    cluster_bool = c(
      "in_cluster",
      "out_cluster",
      "in_cluster",
      "in_cluster",
      "out_cluster",
      "out_cluster",
      "in_cluster",
      "in_cluster",
      "in_cluster")
  )

merged_data <- data_final

pause_ms <- function(merged_data) {
  diff <- merged_data %>%
    group_by(ID, sensor, n_sesion, cluster_bool, bins) %>%
    mutate(t_max = max(tiempo),
           t_min = min(tiempo)) %>%
   subset(select=-tiempo) %>%
   mutate(valid_pause= ifelse(cluster_bool=="out_cluster",NA,"")) %>%
    drop_na() %>%
    unique() %>%
    ungroup() %>%
    mutate(pause_ms = lead(t_min)-t_max)
  
  #left_join(merged_data,diff,by=c("ID", "sensor", "n_sesion", "cluster_bool", "bins"))
}

view(pause_ms(merged_data))