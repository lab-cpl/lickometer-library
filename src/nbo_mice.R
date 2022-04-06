library(tidyverse)

# relevant parameters

# 1. interlick intervals = N(130, 20)
# 2. FR 5
# 3. sacarose bias over time

## ILI ###
ILI <- function(mu){
	return(rnorm(1, mu, 20))
}
## idle time
idle_time <- function(time_){
	return(rnorm(1, time_, 20))
}
## pick from vector at random
behavior <- function(probs){
	p <- probs
	return(sample(c("water", "sucrose", "idle"), 1, replace = TRUE, prob = p))
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
	list(play = function(spout, ILI, idle_time, ID, iterations) {
		     if (spout == "idle"){
			     ILI <- idle_time
			     spout <- sample(c("water", "sucrose"), 1, replace = TRUE, prob = c(0.5, 0.5))
		     }
		     for (i in 1:iterations){
		     total_time <<- total_time + ILI
		     if(spout == "sucrose"){
			     timeout_suc <<- timeout_suc - ILI
			     timeout_wat <<- timeout_wat - ILI
			     est <<- append(est, "sucrose")
			     intervals <<- append(intervals, ILI)
			     licks_suc <<- append(licks_suc, 1)
			     licks_wat <<- append(licks_wat, 0)
			     valid_lick_suc <<- append(valid_lick_suc, 1)
			     valid_lick_wat <<- append(valid_lick_wat, 0)
			     if(timeout_suc > 0){
				     valid_lick_suc <<- valid_lick_suc * 0
			     }
			     if ( ((tail(cumsum(valid_lick_suc), 1)) %% 5 == 0) && valid_lick_suc != 0){
				     event_suc <<- append(event_suc, 1)
				     event_wat <<- append(event_wat, 0)
				     timeout_suc <<- 20000
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
			     valid_lick_suc <<- append(valid_lick_suc, 0)
			     valid_lick_wat <<- append(valid_lick_wat, 1)
			     if(timeout_wat > 0){
				     valid_lick_wat <<- valid_lick_wat * 0
			     }
			     if ( (tail(cumsum(valid_lick_wat), 1) %% 5 == 0) && valid_lick_wat != 0){
				     event_suc <<- append(event_suc, 0)
				     event_wat <<- append(event_wat, 1)
				     timeout_wat <<- 20000
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
				   sensor = as.numeric(as.factor(est)) - 1,
				   total_time = total_time,
				   tiempo = intervals %>% cumsum,
				   licks_suc = licks_suc %>% cumsum,
				   licks_wat = licks_wat %>% cumsum,
				   event_suc = event_suc %>% cumsum,
				   event_wat = event_wat %>% cumsum
				   ) %>%
			    mutate(
				   licks_wat = replace(licks_wat, lag(licks_wat) == licks_wat, 0),
				   licks_suc = replace(licks_suc, lag(licks_suc) == licks_suc, 0),
				   event_wat = replace(event_wat, lag(event_wat) == event_wat, 0),
				   event_suc = replace(event_suc, lag(event_suc) == event_suc, 0),
				   actividad = licks_wat + licks_suc,
				   evento = event_suc + event_wat,
				   exito = event_suc + event_wat,
				   timeout_wat = timeout_wat,
				   timeout_suc = timeout_suc
				   ) %>%
			    select(
				   -licks_wat, -licks_suc,
				   -event_suc, -event_wat,
			    	   -timeout_wat, -timeout_suc,
				   -total_time,
				   ID,
				   sensor,
				   actividad,
				   evento,
				   exito
			    )
			    )}
}})
}


# simulation parameters
n_sims <- 2000
n_mice <- 10
mice <- seq(1, n_mice, 1)
lick_speed <- rnorm(n_mice, 125, 20) %>% abs
idle_ <- rnorm(n_mice, 10000, 20) %>% abs
probs <- c(0.4, 0.5, 0.1) # water/sucrose/idle
# run simulation
list(mice, lick_speed, idle_) %>%
	pmap_dfr(., function(m, l, i){
		    # for each mice a session is simulated
		    sim <- simulate_session()
		    sim$play(
		       behavior(probs),
		       ILI(l),
		       idle_time(i),
		       m,
		       n_sims
		       ) -> out
		    return(out)
}
	) -> simulation_results

simulation_results %>%
	write_csv("../test/files/simulation.csv")

