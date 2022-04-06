library(tidyverse)

# relevant parameters

# 1. interlick intervals = N(130, 20)
# 2. FR 5
# 3. sacarose bias over time

## ILI ###
ILI <- function(){
	return(rnorm(1, 130, 20))
}
## idle time
idle_time <- function(){
	return(rnorm(1, 10000, 20))
}
## pick from vector at random
behavior <- function(){
	probs <- c(0.3, 0.6, 0.1)
	return(sample(c("water", "sucrose", "idle"), 1, replace = TRUE, prob = probs))
}

## FR ##
# if trigerred five times a reward is produced
spouts <- function(){
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
	list(events = function(spout, ILI, idle_time) {
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
		     else if (spout == "idle"){
			     if (length(est) == 0){
				     # idle time
				     timeout_suc <<- timeout_suc - idle_time
				     timeout_wat <<- timeout_wat - idle_time
				     return(NA)
			     }
			     else{
				     # idle time
				     timeout_suc <<- timeout_suc - idle_time
				     timeout_wat <<- timeout_wat - idle_time
			     }
		     }
		     return(
			    tibble(
				   est = est,
				   intervals = intervals,
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
				   licks = licks_wat + licks_suc,
				   events = event_suc + event_wat,
				   timeout_wat = timeout_wat,
				   timeout_suc = timeout_suc,
				   total_time = total_time
				   ) %>%
			    select(
				   -licks_wat, -licks_suc,
				   -event_suc, -event_wat,
			    	   -timeout_wat, -timeout_suc
			    )
			    )
})
}

sp <- spouts()
sp$events(behavior(), ILI(), idle_time())



