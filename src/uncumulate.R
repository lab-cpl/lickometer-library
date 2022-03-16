uncumulate <- function(merged_input){
         merged_input %>%
                 group_by(ID, n_sesion, sensor) %>%
                 mutate(
                        evento_no_acumulado = sum(abs(diff(evento)))
                        ) %>%
                 ungroup()
 }
