# load data

# load files into list
# loads files from a given folder into a list
# checks for expected data types in each column
# takes as parameters path and assumes extension is .csv

# loads data in a directory
# adds date and time columns
load_data <- function(path){
	load_data_as_char(path) %>%
		validate_data()
}
# creates a column with date
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
source_to_date_sec <- function(x) {
	sub('\\..*$', '', basename(x)) %>%
		lubridate::ymd_hms(.) -> out
	return(out)
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
					exito = as.factor(exito),
				        fecha = source_to_date(source),
				        fecha_ms = source_to_date_posix_ms(source),
				        tiempo_fecha_ms = fecha_ms + tiempo,
				        tiempo_fecha = source_to_date_sec(source) + lubridate::seconds(tiempo / 1000)
	       )
}
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
validate_data <- function(data_as_char){
	rules <- validate::validator(
				     field_format(ID, "^[0-9]+$", type = "regex"),
				     field_format(sensor, "^[0-9]$", type = "regex"),
				     field_format(tiempo, "^[0-9]+$", type = "regex"),
				     field_format(actividad, "^[0-9]+$", type = "regex"),
				     field_format(evento, "^[0-9]+$", type = "regex"),
				     field_format(exito, "^[0-9]+$", type = "regex")
				     )
	format_errors <- confront(data_as_char, rules)$._value %>%
		as_tibble()
	error_matrix <- which(!as.matrix(format_errors), arr.ind = TRUE)
	if (length(error_matrix) == 0){
		message("Data contains no format errors")
		return(format_data_type(data_as_char))
	}
	else{
		message("Format errors found in the following rows and columns")
		print(error_matrix)
		return(error_matrix)
	}
	return(NA)
}
