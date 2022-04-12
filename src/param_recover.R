#libraries
pacman::p_load(tidyr,
	       tidyverse,
	       dplyr,
	       purrr,
	       readr,
	       lubridate,
	       validate,
	       ggplot2)

#lickometer_library
source("load_data.R")
source("load_metadata.R")
source("merge_inputs.R")
source("time_activity_correction.R")
source("uncumulate.R")
source("interval_estimate.R")
source("bin_calculation.R")
source("burst_estimates.R")
source("n_clusters.R")
source("pause_ms.R")
source("synch_lick_event.R")


# merge data and metadata
merge_inputs(
	     "../test/files/metadata_example.csv",
	     "../test/files/"
	     ) %>%
time_activity_correction() %>%
uncumulate() -> ds

# recover events per minute per mice
ds %>%
	mutate(evento_no_acumulado = replace_na(evento_no_acumulado, 0)) %>%
	group_by(ID) %>%
	summarise(
		  total_eventos = max(cumsum(evento_no_acumulado)),
		  session_length = max(tiempo) / (1000 * 60),
		  events_per_min = total_eventos / session_length,
		  total_rows = n()
		  )

# recover probability sucrose/water
ds %>%
	mutate(evento_no_acumulado = replace_na(evento_no_acumulado, 0)) %>%
	group_by(ID) %>%
	summarise(
		  total_eventos = max(cumsum(evento_no_acumulado))
		  ) %>%
	left_join(
		  ds %>%
			mutate(evento_no_acumulado = replace_na(evento_no_acumulado, 0)) %>%
			group_by(ID, tipo_recompensa) %>%
			summarise(
				  events_per_sensor = max(cumsum(evento_no_acumulado))
				  )
			  ) %>%
	mutate(
	       emp_prob = round(events_per_sensor / total_eventos, 2)
	       )

