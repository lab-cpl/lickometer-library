#uncumulate
uncumulate <- function(merged_input){
         merged_input %>%
                 group_by(ID, n_sesion, sensor) %>%
                 mutate(
                        evento_no_acumulado = c(0, diff(evento)) %>%
				replace(. == 0, NA),
                        actividad_no_acumulada = c(1, diff(actividad)) %>%
				replace(. == 0, NA)
                        ) %>%
                 ungroup()
}
