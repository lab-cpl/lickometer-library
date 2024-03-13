# libs----
pacman::p_load(
  tidyr,
  tidyverse,
  dplyr,
  purrr,
  readr,
  lubridate,
  validate,
  pointblank,
  furrr
)


# load_data function----
# this function loads .csv files based on _[0-9]+\\.csv regex expression
# tries to coerce variable into proper format and adds further variables
# for data analysis
load_data <- function(path){
  load_data_as_char(path) %>% 
    format_data_type(.)
}


# helper functions for load_data----

# loading data as char allows us to validate data against regex pattern
load_data_as_char <- function(path){
  # Read files
  ds <- list.files(
    path = path,
    # note that this pattern should be unique for lickometer files of interest
    pattern = "_[0-9]+\\.csv",
    full.names = T
  ) %>%
    set_names() %>%
    purrr::map_dfr(
      # columns are read as character to apply regex and avoid unwanted formating
      read_csv, .id = "source", col_types = cols(.default = "c")
    )
  
  # Issue information for files loaded
  warning(paste("The following data files were loaded:", 
                paste(unique(ds$source), collapse = "\n"), 
                sep = "\n"))
  
  # Returns dataset
  return(ds)
}

# correct data type for all relevant variables
format_data_type <- function(raw_file){
  raw_file %>%
    mutate(
      ID = as.factor(ID), # mouse ID
      sensor = as.factor(sensor), # lickometer sensor/spout
      tiempo = as.numeric(tiempo), # ms from start of the program
      actividad = as.numeric(actividad), # cumulative licks per spout
      evento = as.factor(evento), # cumulative events per spout
      exito = as.factor(exito), # boolean, if event gave or not a reward
      fecha = source_to_date(source), # date
      fecha_ms = source_to_date_posix_ms(source), # date in unix epoch
      tiempo_fecha_ms = fecha_ms + tiempo, # date + hms in unix epoch
      tiempo_fecha = source_to_date_sec(source) + lubridate::seconds(tiempo / 1000) # same as above but regular format
    )
}

# iow lickometer files contain the date as part of the filename
# this function extracts that part and adds it back into the tibble
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

# same as source_to_date but keeps the whole format (with seconds)
source_to_date_sec <- function(x) {
  sub('\\..*$', '', basename(x)) %>%
    lubridate::ymd_hms(.) -> out
  return(out)
}

# sames as source_to_date but in posix epoch
source_to_date_posix_ms <- function(x) {
  sub('\\..*$', '', basename(x)) %>%
    lubridate::ymd_hms(.) %>%
    lubridate::seconds(.) %>%
    as.numeric() * 1e3
}

# load_metadata function----

# this functions load and check the metadata of lickometer files
# it tries to ensure that all data is properly formated
load_metadata <- function(x) {
  # x is the path to a csv file that contains all metadata for the experiment. 
  # Check : that column names are correct and with the correct data type.
  # correct column names
  col_names <- c(
    "pool", # pool is the experiment label
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
  )
  # correct regex patterns
  patterns <- c(
    "[a-z]", # pool
    "[0-9]", # n_sesion
    "[0-9]{4} [0-9]{2} [0-9]{2}", # fecha
    "[a-z]+_[a-z]+_[a-z]+_[a-z]+", # droga droga1_droga2_metodo_area
    "[0-9]+_[0-9]+_[a-z]+", # dosis dosis1_dosis2_unidad de medida
    "[0-9]", # ID
    "[0-9]{2} [0-9]{2} [0-9]{2}", # hora_inicio
    "[0-9]{2} [0-9]{2} [0-9]{2}", # hora_termino
    "[0-9]", # n_licometro
    "[a-z]+", # estimulo_spout_1
    "[a-z]+", # estimulo_spout_2
    "[0-9]+", # licks_inicio_spout_1
    "[0-9]+", # licks_inicio_spout_2
    "[0-9]+", # licks_fin_spout_1
    "[0-9]+", # licks_fin_spout_2
    "[0-9]+", # eventos_inicio_spout_1
    "[0-9]+", # eventos_inicio_spout_2
    "[0-9]+", # eventos_fin_spout_1
    "[0-9]+", # eventos_fin_spout_2
    "[0-9]+", # ml_consumidos_spout_1
    "[0-9]+" # ml_consumidos_spout_2
  )
  
  # run check for names and values
  colCheck <- base::sapply(1:length(col_names), function(colNameIndex) {
    colName <- col_names[colNameIndex]
    index <- which(names(x) == colName) # which column in metadata is colName	  
    if(length(index) == 1) { # TRUE if colName exists only once
      #  TRUE if column, except NA matches pattern
      ifelse(all(na.omit(str_detect(x %>% dplyr::pull(colName), patterns[colNameIndex]))), TRUE, FALSE) 
    }
  })
  
  # Create message with results of name and format check
  if(all(colCheck)) {
    message("Import metada ok!!")
  } else {
    stop(paste("WARNING: The following required columns are either not found on the dataset or have the incorrect format:",
               paste(col_names[colCheck==FALSE], collapse = "\n"), 
               sep =  "\n"))
  }
  
  # Process metadata. Select desired columns and format
  out <- x %>% 
    select(all_of(col_names)) %>% 
    filter(if_any(everything(), ~ !is.na(.))) %>% # Filter out all NA rows
    mutate(
      pool = as.factor(pool),
      n_sesion = as.numeric(as.character(n_sesion)),
      fecha = lubridate::ymd(fecha),
      droga = as.character(droga),
      dosis = as.character(dosis),
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
  
  # Check number of NA´s per column
  countNA <- out %>% 
    map(., ~sum(is.na(.))) %>% 
    as_tibble %>% 
    select_if(~ .x != 0) 
  if (any(countNA != 0)) {
    message(paste("WARNING: The following columns of the metadata contain one or more NA values.",
                  paste(names(countNA), collapse = "\n"),
                  sep = "\n")) 
  }
  
  return(out) # return data
}

# load_experiment function----

# load_experiment merges lickometer data with its metadata
# and does further checking of formatting errors
load_experiment <- function(metadataFileName, data_directory_path) {
  # Load meta-data, importing as character allows for regex pattern matching
  if (file.exists(metadataFileName)) {
    metadata <- load_metadata(read_csv(metadataFileName, col_types = cols(.default = "c")))  
  } else {
    stop("The specified metadata file does not exist.")
  }
  
  # Check that meta-data contains a combination of one mice per day
  check <- metadata %>% 
    count(ID, fecha) %>% 
    filter(n != 1) %>% 
    select(ID, fecha) %>% 
    mutate(info = paste(ID, fecha, sep = "|")) %>% 
    pull(info)
  if(length(check) != 0) {
    stop(paste("The following combinations of ID  and date are repeated", 
               paste(check, collapse = "\n"),
               sep = "\n"))
  }
  
  # Load lickometer data
  data_file <- load_data(data_directory_path)
  # IOW labs lickometer data should NOT produce any NA. 
  # an NA is likely data corruption and needs to be informed
  countNA <- data_file %>% 
    filter(if_any(ID:tiempo_fecha, ~is.na(.x)))
  if (nrow(countNA) != 0) {
    message(paste("WARNING: The following lickometer data files contain one or more NA values. This might affect analysis results",
                  paste(names(countNA), collapse = "\n"),
                  sep = "\n")) 
  }	
  
  # Merge data
  out <- data_file %>%
    # ID and fecha should be enough, as long as animals only
    # get 1 experimental session per day
    left_join(metadata, by = c("ID", "fecha"))	%>% 
    # at this point all data should be in the correct format
    # so we add date in posix_ms for time activity correction
    # function
    mutate(
      # hora inicio and hora fin are used to filter out activity
      # not done by the animals
      hora_inicio_ms = paste(fecha, hora_inicio, sep = " ") %>%
        lubridate::ymd_hms() %>%
        lubridate::seconds() %>%
        as.numeric() * 1e3,
      hora_fin_ms = paste(fecha, hora_termino, sep = " ") %>%
        lubridate::ymd_hms() %>%
        lubridate::seconds() %>%
        as.numeric() * 1e3,
      # this allow to identify the potential reward associated with each lick
      tipo_recompensa = if_else(sensor == 0, estimulo_spout_1, estimulo_spout_2))
  
  # Check number of rows of data files that have meta_data
  if(nrow(out) != nrow(data_file)) {
    stop("The dimensions of the merged experiment data and metadata are different from those of the experiment data alone. Please inspect your metadata carefully")
  } 
  
  # Filter valid data for each mice based on the timestamp of licks that is within the start and end time of the session as obtained from the metadata.
  out <- out %>% 
    filter(
      hora_inicio_ms <= tiempo_fecha_ms &
        tiempo_fecha_ms <= hora_fin_ms) %>% 
    mutate(timestamp = tiempo_fecha_ms - hora_inicio_ms) %>%  # This corrects time based on the metadata start of the session
    return(out)
}


# uncumulate function----
# this function transforms cumulative data into binary
# every row of the data is always a lick so actividad is expected to be always 1
# events however are not, a 1 means that an event was triggered
uncumulate <- function(merged_input){
  merged_input %>%
    group_by(ID, n_sesion, sensor) %>%
    arrange(timestamp, .by_group = TRUE) %>% # This is to make sure data is ordered by time_stamp
    mutate(
      # NA_real_ is to preserve dbl class. The use of a nested if_else is necessary so the first activity
      # log doesn't get assigned a NA value. Because events starts at zero, the correction does not apply
      evento_no_cum = if_else(evento - lag(evento) > 0, 1, NA_real_),
      actividad_no_cum = if_else(actividad == min(actividad), 1, 
                                 if_else(actividad - lag(actividad) > 0, 1, NA_real_))
    ) %>%
    ungroup()
}


# interval_estimate function----
# This function creates a new vector that calculates the interval
# for a given time-stamp vector
# note that this function should be used inside a mutate in order to be added
# into the tibble
interval_estimate <- function(x){ x-lag(x) }



# bin_calculation function----
# this function computes bins of n minutes allowing to make
# time course analysis
bin_calculation <- function(merged_data, session_length_min, bin_size_min){
  
  # Check that no data for the corrected time stamp is above the session length
  countCheck <- merged_data %>% 
    mutate(flag = timestamp > session_length_min * 60 * 1e3) %>% 
    filter(flag) %>% # select only the data that is above the session length
    count(ID, fecha, flag) %>% 
    select(!flag) %>% 
    unite("msg", everything(), sep = "\t") %>% 
    pull(msg)
  
  # Display message
  message(paste("WARNING: The following mice have timestamps outside the specificed session length",
                "MICE\tDATE\tTIMESTAMPS",
                paste(countCheck, collapse = "\n"),
                sep = "\n"))
  
  # Calculate bins
  # bins are added to the main tibble
  merged_data %>%
    group_by(
      ID,
      n_sesion
      ) %>%
    mutate(
      # variable timestamp contains time relative to the start of the session,
      # bins are calculated based on the 
      # length of the session
      bins = cut(
        timestamp,
        breaks = seq(0, session_length_min * 60 * 1e3, by = bin_size_min * 60 * 1e3),
        labels = FALSE,
        include.lowest = TRUE))
}

# burst_estimates function----
# this function calculates bursts
# a burst is defined as a cluster of licks that occur within a specified
# time threshold typically from 200 - 1000 ms
burst_analysis <- function(dataset, threshold){ 
  # Checks for presence of interval in dataset
  # this should be done with interval_estimate function inside a mutate
  # note that the column should be named interval
  if(sum(names(dataset) == "interval") != 1) {
    stop("function burst_analysis requires one column named interval that contains intervals to calculate bursts")
  }
  
  # Checks for presence of interval in dataset
  if(sum(names(dataset) == "timestamp") != 1) {
    stop("function burst_analysis requires one column named timestamp that contains intervals to calculate bursts")
  }
  
  # Run function
  burst_ds <- dataset %>%
    group_by(ID, sensor, n_sesion) %>%
    # this determines whether each timestamp belongs in a cluster or not
    mutate(
      cluster_bool = ifelse(interval > threshold, "out_cluster", "in_cluster")
    ) 
  
  # Get burst summaries stats
  burst_summary <- burst_ds %>%
    group_by(ID, sensor, n_sesion, .add = TRUE) %>% 
    group_split() 
  
  burst_summary <- lapply(burst_summary, function(ds) {
    # Fix first assignment
    ds$cluster_bool[1] <- "in_cluster" # First timestamp will always be its own cluster
    
    # Get estimates of burst by using rle R's base function 
    s <- ds %>% 
      group_by(ID, sensor, n_sesion, tipo_recompensa, group, pool) %>% 
      summarise(
        values = rle(cluster_bool)$values, # inside or outside burst
        length = rle(cluster_bool)$lengths) # number of timestamps asssociated with the burst
    
    # Create temporary index variable   
    index <- cumsum(s$length) 
    index2 <- index - (s$length - 1)
    s$burst_timestamp_start <- ds$timestamp[index2] # get timestamp for start of cluster	 
    s$burst_timestamp_end <- ds$timestamp[index] # get timestamp for end of cluster
    return(s)
  }) %>% 
    bind_rows() %>% 
    rename(cluster_length = length)  
  
  # Return data
  return(burst_summary)
}

# synch_lick_event function----
# this function generates peri-event windows
synch_lick_event <- function(ds, parallel){
  # data table makes this function much more faster
  require(data.table)
  
  # Check parallel option is boolean
  if(class(parallel) != "logical") {
    stop("Parallel option has to be logical")
  }
  
  # Check that parallel option argument was provided
  if(parallel) {
    # Print message
    message("Parallel option uses the parallel package")
    require(pbmcapply)
    require(parallel)
  }
  
  # Extract row_index from the main dataset
  # this is to then be able to get back the event position in the main dataset
  ds <- ds %>%
    mutate(rn = row_number())
  row_index <- ds %>% select(rn)
  
  # Iterate over groups
  out <- ds %>%
    group_split(
      ID,
      fecha,
      sensor
    ) %>%
    map_dfr(., function(x){
      # first we get row indices of events
      event_indices <- 	x %>%
        # evento_no_cum == 1 is the lick that triggered the event
        filter(evento_no_cum == 1) %>%
        # we get the row number of each triggered per IDxSessionxSensor
        pull(rn)
      
      # this tibble defines for every event
      # the previous and the next event
      tibble(
        # the start of an event is the end of the previous
        # so the start is equal to the lag (previous) of this list
        # if its the first event then this event is also its start
        start_ = lag(unlist(event_indices), default = min(row_index)),
        # the end of an event is the one that follows
        # if its the last event then this event is also its end
        end_ = lead(unlist(event_indices), default = max(row_index)),
        # idx is the event row number in the original dataset
        idx = event_indices,
        # sensor event tells in which sensor this event was triggered
        sensor_event = unique(x$sensor)
      )
    })
  
  # Run sequence with and without parallel processing
  # split each tibble containing the start and end of each event
  index_list <- split(out, seq(nrow(out))) 
  
  if (parallel) {
    P <- pbmclapply(index_list, function(x) {
      rows <- x$start_[1]:x$end_[1] # This is a list of position of the start and
      # end of each event
      # first we slice the main dataset with that start and indices
      ds[rows,] %>%
        mutate(
          # then we create a column indicating which event this is
          event_id = x$idx[1],
          # in which spout was done
          sensor_event = x$sensor_event[1],
          # we substract the timestamp when the event was triggered
          # so tiempo_peri_evento equals 0 where the event was triggered
          #  the previous timestamps are negative
          # and timestamps posterior to this are positive
          tiempo_peri_evento = timestamp - timestamp[which(rn == x$idx[1])]
        ) %>% 
        # data that is from another spout is excluded
        # this is done after because the animal could've changed the licked
        # spout
        filter(sensor == sensor_event)
    }, mc.cores = 3) %>%
      # we get back a unified dataset which now includes the peri event
      # timestamp for every event
      # this is kinda slow
      data.table::rbindlist(., fill = TRUE) %>% 
      # finally we group by animal and session
      # and transform event_id so it goes form 1 to n
      # note that with this settings event 1 could be sucrose
      # and event 2 could be water
      group_by(ID, fecha) %>% 
      mutate(event_id = as.numeric(as.factor(event_id))) %>% 
      ungroup()
    # bind_rows is considerably slower
    #  bind_rows() %>% 
    #filter(sensor = sensor_event) 
  } else {
    require(pbapply) # package required for progress bar
    P <- pblapply(index_list, function(x) {
      rows <- x$start_[1]:x$end_[1] # This is a list of positions within 
      ds[rows,] %>%
        mutate(
          event_id = x$idx[1],
          sensor_event = x$sensor_event[1],
          tiempo_peri_evento = timestamp - timestamp[which(rn == x$idx[1])]
        ) %>% 
        filter(sensor == sensor_event)
    }) %>% 
      data.table::rbindlist(., fill = TRUE) %>% 
      # finally we group by animal and session
      # and transform event_id so it goes form 1 to n
      # note that with this settings event 1 could be sucrose
      # and event 2 could be water
      group_by(ID, fecha) %>% 
      mutate(event_id = as.numeric(as.factor(event_id))) %>% 
      ungroup()
    # This function is extremely slow
    #  bind_rows() %>%
    # filter(sensor == sensor_event)   
  }
  return(P)   
}

### This function creates a tibble with spec required by beezdemand
### to analyze demand/cost curves
# data = data imported from main lickometer library function
# reward_type = as a string within "" the reward type you are interested
# licks_events = either "licks" or "events"
# demand_curve_sessions = a vector specifying the number of session corresponding
# to demand curve c(1,2,3)
data_for_demand_curve <- function(
        data,
        reward_type,
        licks_events,
        demand_curve_sessions
        ){
    unique_time_bins <-
        data %>% 
        group_by(ID, n_sesion) %>% 
        filter(tipo_recompensa == reward_type) %>% 
        ungroup() %>% 
        mutate(
            n_bin = as.integer(timestamp%/%600000)
        ) %>% 
        group_by(ID, pool, n_sesion, n_bin, droga, dosis) %>% 
        summarise(
            bin_licks = n(),
            bin_events = length(evento %>% unique) - 1
        ) %>% 
        ungroup() %>% 
        group_by(ID, n_sesion) %>%
        arrange(n_bin, .by_group = TRUE) %>% 
        pivot_longer(
            c("bin_licks", "bin_events"),
            names_to = "endpoint",
            values_to = "val") %>%
        ungroup() %>% 
        complete(
            nesting(ID,pool, n_sesion, endpoint, droga, dosis),
            n_bin,
            fill=list(val=0)) %>%
        mutate(price = (case_when( 
          n_bin == 0 ~ 5,
          n_bin == 1 ~ 10,
          n_bin == 2 ~ 20,
          n_bin == 3 ~ 40,
          n_bin == 4 ~ 80,
          n_bin == 5 ~ 120))) %>%
        ungroup() %>% 
        filter(
            endpoint == paste0("bin_", licks_events),
            n_sesion %in% demand_curve_sessions
        )
    out <-
        unique_time_bins %>% 
        rename(
            C = price,
            Q = val
        ) %>% 
        mutate(
            ID = paste(ID, droga, dosis, n_sesion, sep = ".")
        ) %>% 
        select(ID, C, Q)
    return(out)
}


# runs the demand curve fit and parses data into two list
parse_demand_curve_fit <- function(data){
    fit <- beezdemand::FitCurves(
        as.data.frame(data),
        equation = "koff",
        xcol = "C",
        ycol = "Q",
        idcol = "ID",
        detailed = TRUE
    )
    full <- fit
    tibble_out <- as_tibble(fit$dfres) %>% 
        separate_wider_delim(id, ".", names = c("ID", "droga", "dosis", "n_sesion"))
    fits <- fit$fits
    fit_out <- 1:length(fits) %>% 
        map_dfr(
            ., purrr::possibly(
                function(f){
                ID <- names(fits[f])
                df <- fits[[f]] %>% 
                    broom::tidy() %>% 
                    mutate(
                        ID = ID
                    )
            }
        )
        ) %>% 
        separate_wider_delim(ID, ".", names = c("ID", "droga", "dosis", "n_sesion"))
    list_out <- list(
        parsed = tibble_out,
        fits = fit_out,
        complete = full
    )
    return(list_out)
}

# check for unsystematic responses
check_unsystematic <- function(standard_format_data){
    d <-
        standard_format_data %>% 
        ungroup() %>% 
        separate(ID, c("id", "droga", "dosis", "n_sesion"), sep = "\\.") %>% 
        rename(
            x = C,
            y = Q
        ) %>% 
        group_by(id, droga, dosis, n_sesion) %>% 
        group_split() 
    out <-
        d %>% 
        map(
            ., possibly(
                function(x){
                   # note change parameters as needed
                    beezdemand::CheckUnsystematic(
                        dat = x,
                        deltaq = 0.025,
                        bounce = 0.2,
                        reversals = 1,
                        ncons0 = 2
                    ) %>% 
                        as_tibble() %>% 
                        mutate(
                            drug = x$droga[1],
                            dose = x$dosis[1]
                        )
                }
            )
        )
    return(out)
}

