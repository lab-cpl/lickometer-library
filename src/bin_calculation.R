bin_calculation <- function(merged_data, bin_size_ms){
	merged_data %>%
		group_by(
			 ID, n_sesion
			 ) %>%
		mutate(
		       tiempo_relativo = tiempo - tiempo[1],
		       bins = cut(
				  tiempo_relativo,
				  breaks = c(seq(0, max(tiempo_relativo), bin_size_ms)),
				  labels = FALSE,
				  include.lowest = TRUE
				  )
		       )
}
