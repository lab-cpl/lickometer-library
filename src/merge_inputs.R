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
		# at this point all data should be in the correct format
		# so we add date in posix_ms for time activity correction
		# function
		out %>%
		drop_na() %>% # at this point na means that data does not have metadata
		mutate(
		       hora_inicio_ms = paste(fecha, hora_inicio, sep = " ") %>%
					lubridate::ymd_hms() %>%
					lubridate::seconds() %>%
					as.numeric() * 1e3,
		       hora_fin_ms = paste(fecha, hora_fin, sep = " ") %>%
					lubridate::ymd_hms() %>%
					lubridate::seconds() %>%
					as.numeric() * 1e3 
		) -> out
		print("Data merged!")
		return(out)
	}
}

# run test
merge_inputs("../test/files/metadata_example.csv", "../test/files/") -> kk

# uncumulate
uncumulate <- function(column){
	if (!is.numeric(column)){
		print("Error column must be cummulative numeric")
		return(NA)
	}
	else{
		print("Uncumulate done")
	}
}

uncumulate(kk$evento)
