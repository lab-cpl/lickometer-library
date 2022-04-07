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
	     "../test/files/metadata_simulation.csv",
	     "../test/files"
	     ) %>%
# creates a column with valid and invalid data 1: valid
time_activity_correction() %>%
# binary form of events
uncumulate() %>%
interval_estimate() %>%
bin_calculation(., 600000) %>%
detect_bursts(., 1000) %>%
n_clusters() %>%
pause_ms() -> data_final

# peri-event
data_final %>%
	synch_lick_event() -> peri_event
peri_event %>%
	unnest(rn) -> out

# test plots sucrose vs water per mice
data_final %>%
	group_by(ID, n_sesion, sensor) %>%
	summarise(licks = n()) -> ind
ind %>%
	group_by(sensor, n_sesion) %>%
	summarise(lm_ = mean(licks), err = sd(licks) / sqrt(n())) -> g
g %>%
	ggplot(aes(sensor, lm_, ymin = lm_ - err, ymax = lm_ + err)) +
	geom_col() +
	geom_errorbar() +
	geom_point(inherit.aes = FALSE, aes(sensor, licks), data = ind) +
	theme_minimal() -> a1
a1

# ILI over bins
data_final %>%
	drop_na() %>%
	filter(valido == 1) %>%
	group_by(sensor, bins) %>%
	summarise(m = mean(interval_estimate), s = sd(interval_estimate)) %>%
	ggplot(aes(bins, m, ymin = m - s, ymax = m + s, fill = sensor)) +
	geom_col(position = "dodge") +
	geom_errorbar(position = position_dodge(0.9)) +
	theme_minimal()

data_final %>%
	filter(sensor == "water") %>%
	select(bins) %>% unique

# mean licks over bins
data_final %>%
	filter(valido == 1) %>%
	drop_na() %>%
	group_by(sensor, bins, ID) %>%
	summarise(l = n()) %>%
	ungroup() %>%
	group_by(sensor, bins) %>%
	summarise(m = mean(l), s = sd(l)) %>%
	ggplot(aes(bins, m, ymin = m - s, ymax = m + s, fill = sensor)) +
	geom_col(position = "dodge") +
	geom_errorbar(position = position_dodge(0.9)) +
	theme_minimal()

# total licks per session
data_final %>%
	filter(valido == 1) %>%
	group_by(sensor, n_sesion, ID) %>%
	summarise(l = n())

