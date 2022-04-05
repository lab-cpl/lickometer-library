#libraries

pacman::p_load(tidyr,tidyverse,dplyr,purrr,readr,lubridate,validate,ggplot2)

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

# merge data and metadata
merge_inputs(
	     "../test/files/metadata_example.csv",
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


# create a csv file to check for possible errors
data_final %>%
	write_csv("../test/files/merged_example.csv")

# ILI histogram
data_final %>%
  filter(interval_estimate <= 1000) -> plot_data

plot_data %>%
	ggplot(aes(interval_estimate)) +
	geom_histogram()
