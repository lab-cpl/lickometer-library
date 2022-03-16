# time_activity_correction
# v 1 0

library(tidyverse)
library(dplyr)
library(lubridate)

# 1: import data from merged -csv
# 2: add time of each activity to the starting time of the program
#    (done as column "tiempo_fecha_ms")
# 3: compare time of each activity to start and end time of each session
# 4: add a column that contains binary data for valid/non-valid rows
#    (0: no 1:yes)

# hora_inicio(ms) + fecha_ms <= tiempo_fecha_ms < hora_termino(ms) + fecha_ms

merged_data <-
  data.frame(read.csv(
    "~/GitHub/lickometer-library/test/files/merged_example.csv"
  ))

merge_date_time_ms <- function(date,time) {
  paste(date, time, sep = " ") %>%
    lubridate::ymd_hms() %>% 
    lubridate::seconds() %>%
    as.numeric() * 1e3
}

time_activity_correction <-
  merged_data %>% mutate(
    inicio_ms = merge_date_time_ms(fecha,hora_inicio),
    termino_ms = merge_date_time_ms(fecha,hora_termino),
    valido = ifelse(
      (inicio_ms <= tiempo_fecha_ms) & (tiempo_fecha_ms < termino_ms),
      "1",
      "0"
    )
  )

