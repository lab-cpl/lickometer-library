# load metadata

library(tidyverse)

load_metadata <- function(x){
		tryCatch(
			 {
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
		       )
			 },
		warning = function(w){
			message("Please check column data format")
		}
		)
}

read_csv("../test/files/metadata_example.csv") -> a

load_metadata(a) -> b



