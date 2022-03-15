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

# hora_inicio + fecha_ms <= tiempo_fecha_ms < hora_termino + fecha_ms

merged_data <-
  data.frame(read.csv(
    "~/GitHub/lickometer-library/test/files/merged_example.csv"
  ))

time_activity_correction <-
  merged_data %>% mutate(
    inicio_ms = as.numeric(seconds(hms(hora_inicio))) + fecha_ms,
    termino_ms = as.numeric(seconds(hms(hora_termino))) + fecha_ms,
    valido = ifelse(
      (inicio_ms <= tiempo_fecha_ms) & (tiempo_fecha_ms < termino_ms),
      "0",
      "1"
    )
  )
