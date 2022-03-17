source("load_data.R")
source("load_metadata.R")
source("merge_inputs.R")
source("time_activity_correction.R")
source("uncumulate.R")

# merge data and metadata
merge_inputs(
	     "../test/files/metadata_example.csv",
	     "../test/files"
	     ) %>%
# creates a column with valid and invalid data 1: valid
time_activity_correction() %>%
# binary form of events
uncumulate() -> data_final

# create a csv file to check for possible errors
data_final %>%
	write_csv("../test/files/merged_example.csv")
