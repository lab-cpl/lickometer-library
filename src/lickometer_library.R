#libraries

library(pacman)
pacman::p_load()

packs <- c("tidyr","tidyverse","dplyr","purrr","readr","lubridate","validate")
success <- suppressWarnings(sapply(packs, require, character.only = TRUE))
install.packages(names(success)[!success])
sapply(names(success)[!success], require, character.only = TRUE)

#lickometer_library

source("load_data.R")
source("load_metadata.R")
source("merge_inputs.R")
source("time_activity_correction.R")
source("uncumulate.R")
source("interval_estimate.R")

# merge data and metadata
merge_inputs(
	     "../test/files/metadata_example.csv",
	     "../test/files"
	     ) %>%
# creates a column with valid and invalid data 1: valid
time_activity_correction() %>%
# binary form of events
uncumulate() %>%
interval_estimate() -> data_final


# create a csv file to check for possible errors
data_final %>%
	write_csv("../test/files/merged_example.csv")
