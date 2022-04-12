# Main function
load_data <- function(path){
	load_data_as_char(path) %>%
		validate_data()
}

# source is the complete path with the filename that contains the date of
# data collection, this function extracts such date and transforms it 
# into the proper format
source_to_date <- function(x) {
	sub('\\..*$', '', basename(x)) %>%
		lubridate::ymd_hms(.) -> tmp
	out <- lubridate::ymd(
			      paste(
				    lubridate::year(tmp),
				    lubridate::month(tmp),
				    lubridate::day(tmp)
				    )
			      )
	return(out)
}

# same as source_to_date but keeps the whole format (with seconds)
source_to_date_sec <- function(x) {
	sub('\\..*$', '', basename(x)) %>%
		lubridate::ymd_hms(.) -> out
	return(out)
}

# sames as source_to_date but in posix epoch
source_to_date_posix_ms <- function(x) {
	sub('\\..*$', '', basename(x)) %>%
		lubridate::ymd_hms(.) %>%
		lubridate::seconds(.) %>%
		as.numeric() * 1e3
}

# correct data type for all relevant variables
format_data_type <- function(raw_file){
			raw_file %>%
				mutate(
					ID = as.factor(ID),
					sensor = as.factor(sensor),
					tiempo = as.numeric(tiempo),
					actividad = as.numeric(actividad),
					evento = as.numeric(evento),
					exito = as.factor(exito),
				        fecha = source_to_date(source),
				        fecha_ms = source_to_date_posix_ms(source),
				        tiempo_fecha_ms = fecha_ms + tiempo,
				        tiempo_fecha = source_to_date_sec(source) + lubridate::seconds(tiempo / 1000)
	       )
}

# loading data as char allows us to validate data
load_data_as_char <- function(path){
	list.files(
		   path = path,
		   pattern = "_[0-9]+\\.csv",
		   full.names = T
		   ) %>%
	set_names() %>%
	purrr::map_dfr(
			  read_csv, .id = "source", col_types = cols(.default = "c")
			  )
}

# data validation
validate_data <- function(data_as_char){
#	rules <- validate::validator(
#				     field_format(ID, "^[0-9]+$", type = "regex"),
#				     field_format(sensor, "^[0-9]$", type = "regex"),
#				     field_format(tiempo, "^[0-9]+$", type = "regex"),
#				     field_format(actividad, "^[0-9]+$", type = "regex"),
#				     field_format(evento, "^[0-9]+$", type = "regex"),
#				     field_format(exito, "^[0-9]+$", type = "regex")
#				     )
#	format_errors <- confront(data_as_char, rules)$._value %>%
#		as_tibble()
#	error_matrix <- which(!as.matrix(format_errors), arr.ind = TRUE)
#	if (length(error_matrix) == 0){
#		message("Data contains no format errors")
#		return(format_data_type(data_as_char))
#	}
#	else{
#		message("Format errors found in the following rows and columns")
#		print(error_matrix)
#		return(error_matrix)
#	}
<<<<<<< HEAD
#	return(NA)
=======
>>>>>>> nbo_mice.R
	return(format_data_type(data_as_char))
}
