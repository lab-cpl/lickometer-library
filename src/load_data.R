# load data

# load files into list
# loads files from a given folder into a list
# checks for expected data types in each column
# takes as parameters path and assumes extension is .csv

library(tidyverse)

# loads data into list
load_files_into_list <- function(path){
	list.files(
		   path = path,
		   pattern = ".csv",
		   full.names = T
		   ) %>%
	set_names() %>%
	purrr::map_dfr(
			  read_csv, .id = "source"
			  ) %>%
	mutate(
	       fecha = source_to_date(source),
	       fecha_ms = source_to_date_posix_ms(source),
	       tiempo_fecha_ms = fecha_ms + tiempo,
	       tiempo_fecha = fecha + lubridate::seconds(tiempo / 1000)
	       ) %>%
	format_data_type(.)
}
# creates a column with date
source_to_date <- function(x) {
	sub('\\..*$', '', basename(x)) %>%
		lubridate::ymd_hms(.)
}
# creates a column with posix date in ms
source_to_date_posix_ms <- function(x) {
	sub('\\..*$', '', basename(x)) %>%
		lubridate::ymd_hms(.) %>%
		lubridate::seconds(.) %>%
		as.numeric() * 1e3
}
# formats data type
format_data_type <- function(raw_file){
			raw_file %>%
				mutate(
						ID = as.factor(ID),
						sensor = as.factor(sensor),
						tiempo = as.numeric(tiempo),
						actividad = as.numeric(actividad),
						evento = as.numeric(evento),
						exito = as.factor(exito)
			   )
}

load_files_into_list("/home/nicoluarte/lab-cpl/lickometer-library/test/files")

