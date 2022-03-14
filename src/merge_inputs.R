# merge inputs

library(tidyverse)
source("load_data.R")
source("load_metadata.R")

merge_inputs <- function(metadata, data_directory){
	metadata <- load_metadata(
				  read_csv(metadata)
				  )
	data_file <- load_data(data_directory)
	data_file %>%
		left_join(metadata, by = c("ID", "fecha")) -> out
	error_check <- names(which(colSums(!is.na(out)) == 0))
	if(length(error_check) != 0){
		print("Possible errors in cols...")
		print(error_check)
	}
	else{
		print("Data merged!")
		return(out)
	}
}
