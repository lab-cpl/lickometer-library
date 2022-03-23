detect_bursts <- function(data_final, threshold){
	data_final %>%
		group_by(ID, sensor, n_sesion) %>%
		mutate(
		       cluster_bool = if_else(interval_estimate > threshold,
					      "out_cluster", "in_cluster") %>% as.factor()
		       ) %>%
		mutate(valid_cluster = if_else(
						   cluster_bool == "in_cluster" &
						   lead(cluster_bool) == "out_cluster" &
							   lag(cluster_bool) == "out_cluster", FALSE, TRUE)) %>%
		ungroup()
}
