library(purrr)
library(tidyquant)
library(tidyverse)
library(parallel)
library(future.apply)

# load functions from helper scripts
getwd() %>%
	list.files(full.names = TRUE) %>%
	str_subset("^(?!.*master.R$)") %>% str_subset(".R$") %>%
	walk(source)


# Q-Learning --------------------------------------------------------------

# function to choose action based on e-greedy policy
select_action_id <- function(Q, action_id, epsilon, m) {
	
	# choose action based on current policy
	# draw from binomial distribution to determine whether to explore or exploit
	if(rbinom(n = 1, size = 1, prob = epsilon)) {
		
		# exploration: choose random action
		selected_action_id <- sample(1:m, 1)
	} else {
		
		# exploitation: choose optimal action
		selected_action_id <- which.max(Q[action_id,])
	}
	return(selected_action_id)
}


# define function for a single run (Q-Learning):

single_run <- function(n = 2,   # number of players
							  zeta = 0.1, # deviation above monopoly price and below one shot nash price
							  m, # number of discrete prices
							  delta, # discount factor,
							  TT = 1e6, # time periods
							  TT_intervention = 10, # time periods after manual intervention
							  alpha, # update rule,
							  beta, # exploration control,
							  seed = NA, # seed for individual run,
							  run_id = NA, # identification of run for apply functions,
							  ... # further arguments passed to economic environment
							  ) {
	
	# calculate discrete set of prices
	p_n <- nash_prices(n, ...)
	p_m <- optimize_joint_profits(n, ...) %>% mean()
	available_prices <- seq(from = p_n - zeta*(p_m - p_n), to = p_m + zeta * (p_m - p_n), length.out = m)
	
	# calculate reward set as response to cartesian product of available prices
	R <- cross2(available_prices, available_prices) %>%
		map(as.numeric) %>% 
		map(calculate_profits, ...)
	
	# initilaize Q matrix
	initial_Q_values <- map_dbl(1:length(available_prices), init_Q_entry, available_prices = available_prices, delta = delta, ...)
	init_Q <- matrix(rep(initial_Q_values, times = m^2), ncol = m, byrow = TRUE)
	Qs <- list(init_Q, init_Q)
	
	# pre-determine time-declining exploration rate
	epsilon <- exp(-beta * 1:TT)
	
	#Set up trackers
	t <- 1  # tracks time
	outcomes <- matrix(NA, nrow = TT + TT_intervention, ncol = n * 2,
							 dimnames = list(NULL, map(c("price", "profit"), ~str_c(., 1:n, sep = "_")) %>% unlist()))
	
	# sample initial actions
	if (!is.na(seed)) {set.seed(seed)}
	initial_actions <- sample(1:m, n, replace = TRUE)
	s_t <- map(0:(n-1), shifter, x = initial_actions) %>%
		map_dbl(get_state_id, m = m)
	
	while (t <= TT) {
		
		# select actions from all players based on current policies
		selected_action_ids <- map2_dbl(.x = Qs,
												  .y = s_t,
												  .f = select_action_id,
												  epsilon = epsilon[t],
												  m = m)
		
		# get price according to action's id
		selected_actions <- available_prices[selected_action_ids]
		
		# retrieve profits from list
		r <- R[[selected_action_ids[1] + (selected_action_ids[2]-1) * m]]
		
		# observe next stage status (i.e. collect price choice id's)
		s_t2 <- map(0:(n-1), shifter, x = selected_action_ids) %>%
			map_dbl(get_state_id, m = m)
		
		# update Q-matrix entries
		for (a in seq_along(Qs)) {
			Qs[[a]][s_t[a], selected_action_ids[a]] <- (1-alpha) * Qs[[a]][s_t[a], selected_action_ids[a]] + alpha * (r[a] + delta * max(Qs[[a]][s_t2[a],]))
		}
		
		# record prices and profits
		outcomes[t,] <- c(selected_actions, r)
		
		# update state & timer
		s_t <- s_t2
		t <- t + 1
	}
	
	### manual intervention
	
		# second agent adheres to learned Q-matrix (full exploitation)
	selected_action_ids[2] <- select_action_id(Q = Qs[[2]], action_id = s_t[[2]], epsilon = 0, m = m)
	
		# first agent examines direct rewards and chooses maximum
	R_available <- (selected_action_ids[2]-1) * m + 1:m
	selected_action_ids[1] <- R[R_available] %>% map_dbl(first) %>% which.max()
	
		# get price according to action's id
	selected_actions <- available_prices[selected_action_ids]
	
		# retrieve profits from list
	r <- R[[selected_action_ids[1] + (selected_action_ids[2]-1) * m]]
	
		# record prices and profits
	outcomes[t,] <- c(selected_actions, r)
	
		# observe next stage status (i.e. collect price choice id's)
	s_t <- map(0:(n-1), shifter, x = selected_action_ids) %>%
		map_dbl(get_state_id, m = m)
	
		# update timer
	t <- t + 1
	
	
	### after manual intervention
	
	while(t <= TT + TT_intervention) {
		
		# both players fully exploit learned strategy
	selected_action_ids <- map2_dbl(.x = Qs,
												  .y = s_t,
												  .f = select_action_id,
												  epsilon = 0,
												  m = m)
	
	# get price according to action's id
	selected_actions <- available_prices[selected_action_ids]
	
	# retrieve profits from list
	r <- R[[selected_action_ids[1] + (selected_action_ids[2]-1) * m]]
	
	# record prices and profits
	outcomes[t,] <- c(selected_actions, r)
	
	# observe next stage status (i.e. collect price choice id's)
	s_t <- map(0:(n-1), shifter, x = selected_action_ids) %>%
		map_dbl(get_state_id, m = m)
	
	# update timer
	t <- t + 1
	
	}
	
	
	return(list(outcomes = outcomes,Q = Qs, timestamp = Sys.time(), run_id = run_id))
}


single_res <- single_run(n = 2,
			  seed = 1234567,
			  zeta = 0.1,
			  m = 15,
			  delta = 0.95,
			  TT = 1200000,
			  TT_intervention = 20,
			  alpha = 0.1,
			  beta = 5*10^-6,
			  c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)

# retrieve number of cores and specify required number of runs
(no_of_cores <- detectCores(all.tests = TRUE, logical = FALSE))
run_ids <- 1:no_of_cores

# run simulations on cluster with specified number of cores
plan(strategy = cluster, workers = no_of_cores)
meta_res <- future_lapply(X = run_ids,
								  FUN = single_run,
								  future.seed = 1234567,
								  n = 2,
								  zeta = 0.1,
								  m = 15,
								  delta = 0.95,
								  TT = 2500000,
								  TT_intervention = 15,
								  alpha = 0.1,
								  beta = 5*10^-6,
								  seed = NA,
								  c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)

str(meta_res)


single_simulation_outcomes <- meta_res[[1]]$outcomes
dim(single_simulation_outcomes)
tail(single_simulation_outcomes)

metrics <- dimnames(single_simulation_outcomes)[[2]]
as_tibble(single_simulation_outcomes) %>%
	mutate(t = row_number(),
			 t_group = (t-1) %/% 1000 + 1) %>%
	group_by(t_group) %>%
	summarize_at(metrics, mean) %>%
	pivot_longer(cols = metrics, names_to = "type", values_to = "value") %>%
	ggplot(aes(x = t_group, y = value)) +
	geom_line() +
	facet_wrap(~type, nrow = 3, scales = "free_y") +
	theme_tq()



as_tibble(single_simulation_outcomes) %>%
	mutate(t = row_number()) %>%
	filter(t > 2499980) %>%
	pivot_longer(cols = metrics, names_to = "type", values_to = "value") %>%
	ggplot(aes(x = t, y = value)) +
	geom_line() +
	facet_wrap(~type, nrow = 3, scales = "free_y") +
	theme_tq()


save(meta_res, file = "simulation_results/first_intervention.RData")
rm(list = ls())
load("simulation_results/first_intervention.RData")
str(meta_res)



# Benchmarking ------------------------------------------------------------

library(doParallel)
library(doFuture)
library(microbenchmark)
TTT <- 200000


local_cluster <- makeCluster(no_of_cores)   # cumbersome clusterExport can be avoided on Linux machines by passing type = "FORK" in makeCluster
clusterExport(local_cluster, c("map2_dbl", "%>%", "cross2", "map", "map_dbl", "str_c",
										 as.vector(lsf.str())))


doParallel_function <- function() {
	registerDoParallel(local_cluster)
	
	res <- foreach(s = seeds) %dopar% single_run(n = 2,
																zeta = 0.1,
																seed = s,
																m = 15,
																delta = 0.95,
																TT = TTT,
																alpha = 0.1,
																beta = 5*10^-6,
																c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)
	return(res)
}



doFuture_function <- function() {
	registerDoFuture()
	
	plan(cluster, workers = 4)
	
	res <- foreach(s = seeds) %dopar% single_run(n = 2,
																zeta = 0.1,
																seed = s,
																m = 15,
																delta = 0.95,
																TT = TTT,
																alpha = 0.1,
																beta = 5*10^-6,
																c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)
	return(res)
}




(benchmarked <- microbenchmark(
	parLapply_res =  parallel::parLapply(cl = local_cluster, X = seeds, fun = single_run,
													 n = 2,
													 zeta = 0.1,
													 m = 15,
													 delta = 0.95,
													 TT = TTT,
													 alpha = 0.1,
													 beta = 5*10^-6,
													 c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25),
	
	foreach_doParallel = doParallel_function(),
	
	foreach_doFuture = doFuture_function(),
	
	future_lapply_res = future_lapply(seeds, FUN = single_run,
												 n = 2,
												 zeta = 0.1,
												 m = 15,
												 delta = 0.95,
												 TT = TTT,
												 alpha = 0.1,
												 beta = 5*10^-6,
												 c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25),
	
	
	
	times = 1
))

stopCluster(local_cluster)


# Profiling ---------------------------------------------------------------


library(profvis)
profvis({
	res <- single_run(n = 2,
							zeta = 0.1,
							m = 15,
							delta = 0.95,
							TT = 1200000,
							alpha = 0.1,
							beta = 5*10^-6,
							seed = 1234567,
							c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)
})

