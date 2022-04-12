#uncumulate
uncumulate <- function(merged_input){
         merged_input %>%
                 group_by(ID, n_sesion, sensor) %>%
                 mutate(
                        evento_no_acumulado = if_else(evento - lag(evento) > 0, 1, NA_real_),
                        actividad_no_acumulada = if_else(actividad - lag(actividad) > 0, 1, NA_real_)
                        ) %>%
                 ungroup()
}
