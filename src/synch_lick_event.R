library(tidyverse)

x <- c(NA, 1, 1, NA, NA, 1, NA, NA , 1)
y <- c(100, 150, 200, 330, 450, 600, 800, 950, 1000)
ID <- c(111, 111, 222, 111, 222, 222, 222, 222, 222)
n_sesion <- c(1, 1, 1, 1, 1, 1, 1, 1 , 1)
sensor <- c(1, 1, 1, 1, 1, 1, 1, 1 , 1)
tt <- rnorm(9 , 100, 1)
ta <- tibble(x = x, y = y, ID = ID, n_sesion = n_sesion, sensor = sensor)

peri_events <- function(merged_data, animal_number, n_sesion_, sensor_){
	merged_data %>%
		mutate(rn = row_number()) %>%
		filter(x == 1,
		       ID == animal_number,
		       n_sesion == n_sesion_,
		       sensor == sensor_
		       ) %>%
		select(rn) %>%
		as.list() %>%
		unlist() %>%
		set_names(., paste0, "_ev") %>%
		map_dfr(
			function(x){
				as.numeric((y - y[x]))
			}
			) %>%
		bind_cols(., merged_data) %>%
		nest(event_time_diff = contains("_ev")) %>%
		mutate(
		       rn = row_number()
		       )
#		map_dfr(function(x) {
#				dd <- merged_data %>% filter(ID == ID)
#				as.numeric(abs(dd$y - dd$y[x]))})
#	bind_cols(., merged_data %>%
#		  mutate(rn = row_number()) %>%
#		  filter(ID == ID)) %>%
#	nest(event_list = contains("_ev")) %>%
#	mutate(ev = lapply(event_list, function(x) min(x))) %>%
#	unnest(ev) %>%
#	mutate(ev = if_else(ev <= window_ms, 1, 0)) -> out
#	return(out)
}
peri_events(ta, 222, 1, 1) %>% unnest(event_time_diff)
