#libraries
pacman::p_load(tidyr,
	       tidyverse,
	       dplyr,
	       purrr,
	       readr,
	       lubridate,
	       validate,
	       pointblank)

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
bin_calculation(., 60000) %>%
detect_bursts(., 1000) %>%
n_clusters() %>%
pause_ms() -> data_final

# peri-event
data_final %>%
	synch_lick_event() -> peri_event

peri_event %>%
	unnest(rn) -> out
