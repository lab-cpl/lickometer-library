synch_lick_event <- function(ds){
plan(multisession, workers = 4)
ds %>%
	mutate(rn = row_number()) -> ds
ds %>% select(rn) -> row_index
ds %>%
	group_split(
		    ID,
		    n_sesion,
		    sensor
		    ) %>%
	map_dfr(., function(x){
			x %>%
				filter(evento_no_acumulado == 1) %>%
				select(rn) -> event_indices
		       tibble(
			      start_ = lag(unlist(event_indices), default = min(row_index)),
			      end_ = lead(unlist(event_indices), default = max(row_index)),
			      idx = event_indices
			      )
}) -> out
	return(
	bind_rows(
	as.list(as.data.frame(t(out))) %>%
		future_map(., function(x){
				rows <- unlist(c(slice(row_index, x[1]:x[2])))
				ds[rows,] %>%
					mutate(
					       event_id = x[3],
					       tiempo_peri_evento = tiempo - tiempo[which(rn == x[3])]
					) %>% filter(sensor == ds$sensor[x[3]])
			      })
	)
	)
}
