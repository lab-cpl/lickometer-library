# load metadata
load_metadata <- function(x){
		x %>% mutate(
		       n_sesion = as.factor(n_sesion),
		       fecha = lubridate::ymd(fecha),
		       droga = as.character(droga),
		       dosis = as.numeric(dosis),
		       ID = as.factor(ID),
		       hora_inicio = lubridate::hms(hora_inicio),
		       hora_termino = lubridate::hms(hora_termino),
		       n_licometro = as.factor(n_licometro),
		       estimulo_spout_1 = as.character(estimulo_spout_1),
		       estimulo_spout_2 = as.character(estimulo_spout_2),
		       licks_inicio_spout_1 = as.numeric(licks_inicio_spout_1),
		       licks_inicio_spout_2 = as.numeric(licks_inicio_spout_2),
		       licks_fin_spout_1 = as.numeric(licks_fin_spout_1),
		       licks_fin_spout_2 = as.numeric(licks_fin_spout_2),
		       eventos_inicio_spout_1 = as.numeric(eventos_inicio_spout_1),
		       eventos_inicio_spout_2 = as.numeric(eventos_inicio_spout_2),
		       eventos_fin_spout_1 = as.numeric(eventos_fin_spout_1),
		       eventos_fin_spout_2 = as.numeric(eventos_fin_spout_2),
		       ml_consumidos_spout_1 = as.numeric(ml_consumidos_spout_1),
		       ml_consumidos_spout_2 = as.numeric(ml_consumidos_spout_2)
		       ) -> out
		val_names(x, c(
			       "n_sesion",
			       "fecha",
			       "droga",
			       "dosis",
			       "ID",
			       "hora_inicio",
			       "hora_termino",
			       "n_licometro",
			       "estimulo_spout_1",
			       "estimulo_spout_2",
			       "licks_inicio_spout_1",
			       "licks_inicio_spout_2",
			       "licks_fin_spout_1",
			       "licks_fin_spout_2",
			       "eventos_inicio_spout_1",
			       "eventos_inicio_spout_2",
			       "eventos_fin_spout_1",
			       "eventos_fin_spout_2",
			       "ml_consumidos_spout_1",
			       "ml_consumidos_spout_2"
			       ))
		patterns <- c(
			      "[0-9]", # n_sesion
			      "[0-9]{4} [0-9]{2} [0-9]{2}", # fecha
			      "[a-z]+_[a-z]+_[a-z]+_[a-z]+", # droga droga1_droga2_metodo_area
			      "[0-9]", # dosis dosis1_dosis2_unidad de medida
			      "[0-9]", # ID
			      "[0-9]{2} [0-9]{2} [0-9]{2}", # hora_inicio
			      "[0-9]{2} [0-9]{2} [0-9]{2}", # hora_termino
			      "[0-9]", # n_licometro
			      "[a-z]", # estimulo_spout_1
			      "[a-z]", # estimulo_spout_2
			      "[0-9]", # licks_inicio_spout_1
			      "[0-9]", # licks_inicio_spout_2
			      "[0-9]", # licks_fin_spout_1
			      "[0-9]", # licks_fin_spout_2
			      "[0-9]", # eventos_inicio_spout_1
			      "[0-9]", # eventos_inicio_spout_2
			      "[0-9]", # eventos_fin_spout_1
			      "[0-9]", # eventos_fin_spout_2
			      "[0-9]", # ml_consumidos_spout_1
			      "[0-9]" # ml_consumidos_spout_2
		)
		val_cols(x, patterns)
		return(out) 
}

val_names <- function(metadata, c_names){
	if ( sum(c_names == names(metadata)) != length(names(metadata))){
							       stop("error")
}
	else{
		message("Column names validation PASS!")
	}
}
possibly_val_names <- possibly(val_names, otherwise = print("Column names do not match expected ones"))

val_cols <- function(metadata, patterns){
	pmap(
	     list(names(metadata), patterns),
	     function(c_names_, patterns_){
		     metadata %>%
			     col_vals_regex(contains(c_names_), patterns_)
	     }
	     )
	return(message("Metadata check succesful!"))
}
