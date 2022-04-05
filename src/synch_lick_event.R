synch_lick_event <- function(ds){
	ds <- ds %>% mutate(rn = row_number())
ds %>%
	group_split(
		    ID,
		    n_sesion,
		    sensor
		    ) %>%
	map(., function(x) {
		    # get all indices of triggered events per list
		    x %>%
			    filter(evento_no_acumulado == 1) %>%
			    select(rn) -> event_indices
		    x %>%
			    select(rn) -> row_index
		    # get a list per event
		    # each list contains data from previous trigger up
		    # to the next trigger
		    # create list with these ranges
		    event_indices %>%
			    map(
				.,
				function(event_index){
					tibble_list <- list()
					# loop through each event index
					for(i in 1:length(event_index)){
						# if its the first event the
						# starting point should be the
						# first lick within conditions
						if (event_index[i] == min(event_index)){
							start_ = 1
						}
						# otherwise we take the
						# previous event as starting
						# point
						else{
							start_ = event_index[i-1]
							start_ = which(row_index == start_)
						}
						# if its the last event then we
						# take the last lick present in
						# the list
						if (event_index[i] == max(event_index)){
							end_ = max(row_index)
						}
						# otherwise we just take the
						# following event
						else{
							end_ = event_index[i+1]
							end_ = which(row_index == end_)
						}
						# original indices are sliced
						r <- slice(row_index, start_:end_)
						d <- ds[unlist(r), ] %>%
							mutate(
							       tiempo_peri_evento = tiempo - tiempo[which(rn == event_index[i])]
							       )
						tibble_list <- append(tibble_list, list(d))
					}
					return(tibble_list)
				}
				)
		    }) -> out
	bind_rows(out) %>% unnest(rn)
}
