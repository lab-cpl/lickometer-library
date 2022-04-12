library(tidyverse)

## DATA SIM ##

# create fake ILI's
data_sim_lic <- function(nrows, events_per_min, animal){
	events_per_min <- 60000 / events_per_min
	idle_time <- sample(c(rnorm(1, 20000, 1), 0),
			    nrows,
			    prob = c(0.2, 0.8),
			    replace = TRUE)
	ili_ds <- tibble(ILI = rnorm(nrows, 100, 1))
	event_ds <- ili_ds %>%
		mutate(
		       ILI = round(ILI + idle_time),
		       tiempo = cumsum(ILI),
		       event_time = trunc(tiempo / events_per_min) - lag(trunc(tiempo / events_per_min)),
		       sensor = sample(c(0, 1), nrows, prob = c(0.9, 0.1), replace = TRUE),
		       a_sac = cumsum(sensor == 1),
		       a_wat = cumsum(sensor == 0),
		       actividad = if_else(sensor == 1, a_sac, a_wat),
		       e_sac = cumsum(replace_na(sensor == 1 & event_time == 1, 0)),
		       e_wat = cumsum(replace_na(sensor == 0 & event_time == 1, 0)),
		       evento = if_else(sensor == 1, e_sac, e_wat),
		       ID = animal,
		       exito = 1
		       )
	return(event_ds %>%
	       fill(sensor, .direction = "up") %>%
	select(
	       ID,
	       sensor,
	       tiempo,
	       actividad,
	       evento,
	       exito,
	       event_time
	       ))
}

# simulate
params <- list(c(800, 800), c(1, 2), c(1, 2))
params %>%
	pmap_dfr(
	     function(x, y, z) data_sim_lic(x, y, z)
	     ) -> sim
write_csv(sim, "../test/files/20221104_130000.csv")



## DATA SIM ##

## MICE SIM ###
## ILI ###
ILI <- function(mu, decay){
	idle <- sample(c(0, rnorm(1, 10000, 1)), 1, replace = TRUE, prob = c(0.7, 0.3))
	return((rnorm(1, mu, 1) + idle) %>% round)
}
## SOFT MAX ##
softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
  }
  val <- exp(par - Lk)
  return(val)
}
## idle time
idle_time <- function(time_){
	return(rnorm(1, time_, 20))
}
## pick from vector at random
behavior <- function(probs){
	return(sample(c("water", "sucrose"), 1, replace = TRUE, prob = probs))
}

## FR ##
# if trigerred five times a reward is produced
simulate_session <- function(){
	est <- c()
	intervals <- c()
	licks_suc <- c()
	licks_wat <- c()
	event_suc <- c()
	event_wat <- c()
	valid_lick_suc <- c()
	valid_lick_wat <- c()
	timeout_suc <- 0
	timeout_wat <- 0
	total_time <- 0
	out <- tibble()
	list(play = function(spout, lick_speed, ID, iterations) {
		     for (i in 1:iterations){
		     ILI <- ILI(lick_speed, i)
		     total_time <<- total_time + ILI
		     spout <- behavior(probs)
		     if(spout == "sucrose"){
			     timeout_suc <<- timeout_suc - ILI
			     timeout_wat <<- timeout_wat - ILI
			     est <<- append(est, "sucrose")
			     intervals <<- append(intervals, ILI)
			     licks_suc <<- append(licks_suc, 1)
			     licks_wat <<- append(licks_wat, 0)
			     if (timeout_suc <= 0){
				     valid_lick_suc <<- append(valid_lick_suc, 1)
				     valid_lick_wat <<- append(valid_lick_wat, 0)
			     }
			     if ( ( tail(cumsum(valid_lick_suc), 1) %% 5 == 0) &&
				 (i != 1) &&
				 (tail(cumsum(valid_lick_suc), 1) != 0) ){
				     event_suc <<- append(event_suc, 1)
				     event_wat <<- append(event_wat, 0)
				     timeout_suc <<- 20000
				     valid_lick_suc <<- valid_lick_suc * 0
			     }
			     else{
				     event_suc <<- append(event_suc, 0)
				     event_wat <<- append(event_wat, 0)
			     }
		     }
		     else if (spout == "water") {
			     timeout_suc <<- timeout_suc - ILI
			     timeout_wat <<- timeout_wat - ILI
			     est <<- append(est, "water")
			     intervals <<- append(intervals, ILI)
			     licks_suc <<- append(licks_suc, 0)
			     licks_wat <<- append(licks_wat, 1)
			     if(timeout_wat <= 0){
				     valid_lick_suc <<- append(valid_lick_suc, 0)
				     valid_lick_wat <<- append(valid_lick_wat, 1)
			     }
			     if ( ( tail(cumsum(valid_lick_wat), 1) %% 5 == 0) &&
				 (i != 1) &&
				 (tail(cumsum(valid_lick_wat), 1) != 0) ){
				     event_suc <<- append(event_suc, 0)
				     event_wat <<- append(event_wat, 1)
				     timeout_wat <<- 20000
				     valid_lick_wat <<- valid_lick_wat * 0
			     }
			     else{
				     event_suc <<- append(event_suc, 0)
				     event_wat <<- append(event_wat, 0)
			     }
		     }
		     if (i == iterations){
		     return(
			    tibble(
				   ID = ID,
				   sensor = as.factor(est),
				   total_time = total_time,
				   tiempo = intervals %>% cumsum,
				   licks_suc = licks_suc %>% cumsum,
				   licks_wat = licks_wat %>% cumsum,
				   event_suc = event_suc,
				   event_wat = event_wat
				   ) %>%
			    mutate(
				   licks_wat = replace(licks_wat, lag(licks_wat) == licks_wat, 0),
				   licks_suc = replace(licks_suc, lag(licks_suc) == licks_suc, 0),
				   event_wat = replace(event_wat, lag(event_wat) == event_wat, 0),
				   event_suc = replace(event_suc, lag(event_suc) == event_suc, 0),
				   actividad = licks_wat + licks_suc,
				   evento = (event_suc + event_wat),
				   exito = (event_suc + event_wat)
				   ) %>%
			    select(
				   -licks_wat, -licks_suc,
				   event_suc, event_wat,
				   -total_time,
				   ID,
				   sensor,
				   actividad,
				   evento,
				   exito
			    ) %>%
			    group_by(ID, sensor) %>%
			    mutate(
				   evento = cumsum(evento),
				   exito = cumsum(exito)
				   )
			    )}
}})
}


# simulation parameters
n_sims <- 5000
n_mice <- 4
mice <- seq(1, n_mice, 1)
lick_speed <- rnorm(n_mice, 125, 1) %>% abs
probs <- c(0.1, 0.9) # water/sucrose
# run simulation
list(mice, lick_speed) %>%
	pmap_dfr(., function(m, l, i){
		    # for each mice a session is simulated
		    sim <- simulate_session()
		    sim$play(
		       probs,
		       lick_speed,
		       m,
		       n_sims
		       ) -> out
		    return(out)
}
	) -> simulation_results

simulation_results %>%
	write_csv("../test/files/20220101_120000.csv")
