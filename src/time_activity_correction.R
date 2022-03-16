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

# hora_inicio_ms <= tiempo_fecha_ms < hora_fin_ms + fecha_ms


time_activity_correction <- function(merged_data) {
  merged_data %>% mutate(valido = ifelse(
    (hora_inicio_ms <= tiempo_fecha_ms) &
      (tiempo_fecha_ms < hora_fin_ms),
    "1",
    "0"
  ))
}

