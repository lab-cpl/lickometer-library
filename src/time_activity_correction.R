# time_activity_correction
# v 0

library(tidyverse)
library(dplyr)
library(lubridate)

#import data from merged -csv
#add time taken to complete activity and beginning of the program
#compare time of activity to beginning and end time of the procedure
#add a column that contains binary data for valid/non-valid rows (0: no 1:yes)

#hora_inicio+fecha_ms < tiempo_fecha_ms < hora_termino+fecha_ms

merged_data <-
  data.frame(read.csv(
    "~/GitHub/lickometer-library/test/files/merged_example.csv"
  ))

time_activity_correction <-
  merged_data %>% mutate(
    inicio_ms = as.integer(seconds(hms(hora_inicio))) + fecha_ms,
    termino_ms = as.integer(seconds(hms(hora_termino))) + fecha_ms,
    valido = ifelse(
      inicio_ms <= tiempo_fecha_ms &
        tiempo_fecha_ms < termino_ms,
      "0",
      "1"
    )
  )
