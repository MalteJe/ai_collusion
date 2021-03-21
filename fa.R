library(purrr)
library(tidyquant)
library(tidyverse)
library(parallel)
library(future.apply)
library(nnet)
library(viridis)

# load functions from helper scripts
getwd() %>%
	list.files(full.names = TRUE) %>%
	str_subset("^(?!.*master.R$)") %>% 
	str_subset("^(?!.*fa.R$)") %>%
	str_subset(".R$") %>%
	walk(source)



# function approximation --------------------------------------------------------------


estimate_state_action_value <- function(state_set, action, feature_specs, w) {
	x <- get_x(state_set = state_set, action = action, feature_specs = feature_specs)
	sum(x * w)
}


optimize_action <- function(state_set, available_prices, feature_specs, w) {
	opt_id <- map_dbl(.x = available_prices,
							.f = estimate_state_action_value,
							state_set = state_set,
							feature_specs,
							w = w) %>%
		which.is.max()
	
	available_prices[opt_id]
}


optimize_grid <- function(w, price_grid, available_prices, feature_specs) {
	map_dbl(.x = price_grid,
			  .f = optimize_action,
			  available_prices = available_prices,
			  feature_specs = feature_specs, 
			  w = w)
}




# define function for a single run (function approximation):

single_run <- function(algorithm,  # determines type of learning algorithm
							  n = 2,   # number of players
							  zeta = 0.1, # deviation above monopoly price and below one shot nash price
							  rounding_precision = 6, # rounding available prices after x digits
							  m, # number of discrete prices
							  TT = 1e6, # time periods
							  TT_intervention = 10, # time periods after manual intervention
							  Alpha, # update rule,
							  Beta, # exploration control,
							  Gamma, # control speed of update for average reward
							  Delta = 1, # discount factor (usually not used in function approximation)
							  Lambda = 0, # trace decay rate (dutch traces)
							  Epsilon_constant = NA, # epsilon if exploration is constant
							  w_init = 0, # initial weights
							  r_init = 0, # initial 'average reward'
							  seed = NA, # seed for individual run,
							  run_id = NA, # identification of run for apply functions,
							  specifications, # specifications regarding calculation of feature set
							  features_by = "tiling", # calculate features using tiling, polynomial or splines?
							  td_error = "differential",  # use differential reward setting?
							  dutch_traces = FALSE, # logical: use eligibility traces?
							  boltzmann = FALSE,  # logical: use boltzmann to select action probabilistically
							  convergence_chunk_length = 10000, # length of block that is checked against convergence
							  convergence_cycle_length = 10, # what is the maximum cycle length considered
							  convergence_check_frequency = 2000, # how often should convergence be checked
							  ... # further arguments passed to economic environment
) {
	
	#print(as.list(match.call()))
	
	# calculate discrete set of prices
	
	# p_n <- nash_prices(n, ...)
	# p_m <- optimize_joint_profits(n, ...) %>% mean()
	p_n <- 1.472928
	p_m <- 1.924981
	
	# determine feasibe prices
	mc <- list(...)$c[1]
	# TBD: make more general so that two players have different available prices due to varying marginal costs
	max_price <- p_m + zeta * (p_n - mc)
	available_prices <- round(seq(from = mc, to = max_price, length.out = m), rounding_precision)
	
	
	# calculate reward set as response to cartesian product of available prices
	R <- cross2(available_prices, available_prices) %>%
		map(as.numeric) %>% 
		map(calculate_profits, ...)
	
	
	if (features_by == "tiling") {
		get_x <<- get_x_tiling
		
		feature_specs <- set_up_tilings(specifications = specifications, min_price = mc, max_price = max_price)
		
		length_w <- specifications$n_tiles^3 * specifications$n_tilings
	} else if (features_by == "splines") {
		
		get_x <<- get_x_splines
		# get_x <<- get_x_splines2
		
		feature_specs <- set_up_splines(specifications = specifications, min_price = mc, max_price = max_price, rounding_precision = rounding_precision)
		
		length_w <- 6 * (specifications$splines_degree + specifications$n_knots)
		#length_w <- (specifications$splines_degree + specifications$n_knots)^3
	} else if (features_by == "poly") {
		get_x <<- get_x_poly
		
		feature_specs <- set_up_poly(specifications = specifications)
		
		length_states <- n + 1  # '+1' reflects action undertaken
		length_w <- choose(specifications$degree + length_states, length_states) - 1
	} else {
		stop("features_by must be one of 'tiling', 'splines' or 'poly'")
	}
	
	
	w_single <- rep(w_init, length_w)
	w <- list(w_single, w_single)
	
	# initiate eligibility trace
	z_single <- rep(0, length_w)
	z <- list(z_single, z_single)
	
	
	# boltzmann or epsilon-greedy
	if (boltzmann) {
		select_action_id <- select_action_id_boltzmann
	} else {
		select_action_id <- select_action_id_greedy
	}
	
	# pre-determine (time-declining) exploration rate
	if(is.na(Epsilon_constant)) {
		
		if (boltzmann) {
			epsilon <- seq(from = 0.1, to = Beta, length.out = TT)
		} else {
			epsilon <- exp(-Beta * 1:TT)
		}
		
	} else {
		epsilon <- rep(Epsilon_constant, TT)
	}
	
	
	# determine type of TD-Error
	
	if(td_error == "differential") {
		td_error <- td_error_differential
		
		# set up average reward
		r_bar <- rep(r_init, 2)
		
		# initialize argument passed continuously to td_error_differential
		TDs <- list(list(Error = 0, r_bar = r_init, Gamma = Gamma), list(Error = 0, r_bar = r_init, Gamma = Gamma))
	} else if (td_error == "expected") {
		
		# TBD HIER
		
	} else if (td_error == "discounted") {
		td_error <- td_error_discounted
		TDs <- list(list(Error = 0), list(Error = 0))
	} else {
		stop("td_error must be one of 'differential', 'expected' or 'discounted'")
	}
	
	
	# Set up trackers
	t <- 1  # tracks time
	outcomes <- matrix(NA, nrow = TT + TT_intervention, ncol = n * 2,
							 dimnames = list(NULL, map(c("price", "profit"), ~str_c(., 1:n, sep = "_")) %>% unlist()))
	convergence <- list(converged = FALSE)
	
	
	
	
	# sample initial state
	if (!is.na(seed)) {set.seed(seed)}
	initial_state_ids <- sample(1:m, n, replace = TRUE)
	s_t <- available_prices[initial_state_ids]
	
	
	
	environment_initialization <- mget(ls())
	
	if (algorithm == "SARSA") {
		environment_convergence <- SARSA(passed_environment = environment_initialization)
		browser()
	}
	
	list2env(x = environment_convergence, envir = environment())
	
	# 
	# # compute initial action
	# selected_actions <- map(.x = w,
	# 								.f = select_action_id,
	# 								state_set = s_t,
	# 								epsilon = epsilon[t],
	# 								m = m,
	# 								feature_specs = feature_specs,
	# 								available_prices = available_prices) %>%
	# 	transpose() %>% map(unlist)
	# 
	# # selected_actions$value <- available_prices[selected_actions$id]
	# 
	# # extract features from current state-action combination
	# x_t <- map(.x = selected_actions$value,
	# 			  .f = get_x,
	# 			  state_set = s_t,
	# 			  feature_specs = feature_specs)
	# 
	# # initialize state-action-value
	# Q_old <- rep(0, n)
	# 
	# 
	# print("before")
	# 
	# while (convergence$converged == FALSE && t < TT) {
	# 	
	# 	# retrieve profits from list
	# 	r <- R[[selected_actions$id[1] + (selected_actions$id[2] - 1) * m]]
	# 	
	# 	# record prices and profits
	# 	outcomes[t,] <- c(selected_actions$value, r)
	# 	
	# 	# move to next time stage
	# 	t <- t + 1
	# 	
	# 	# past actions become today's status
	# 	s_t2 <- selected_actions$value
	# 
	# 	# select actions from all players based on current policies
	# 	selected_actions2 <- map(.x = w,
	# 										 .f = select_action_id,
	# 										 state_set = s_t2,
	# 										 epsilon = epsilon[t],
	# 										 m = m,
	# 										 feature_specs = feature_specs,
	# 										 available_prices = available_prices) %>%
	# 		transpose() %>%
	# 		map(unlist)
	# 	# selected_actions2$id[2] <- m - 3
	# 	
	# 	# selected_actions2 <- available_prices[selected_action_ids2]
	# 	
	# 	# extract features from current state-action combination
	# 	x_t2 <- map(.x = selected_actions2$value,
	# 					.f = get_x,
	# 					state_set = s_t2,
	# 					feature_specs = feature_specs)
	# 	
	# 	
	# 	# calculate "quality" of selected actions given states
	# 	
	# 		#previous episode
	# 	Q_t <- map2_dbl(.x = x_t,
	# 					.y = w,
	# 					.f = ~sum(.x * .y))
	# 	
	# 		# current episode
	# 	Q_t2 <- map2_dbl(.x = x_t2,
	# 					 .y = w,
	# 					 .f = ~sum(.x * .y))
	# 	
	# 	
	# 	# if(t %% 200 == 0) {browser()}
	# 	
	# 	# update weights and average reward
	# 	for (a in seq_along(w)) {
	# 		
	# 		
	# 		# calculate TD-error 
	# 		TDs[[a]] <- td_error(r = r[a], Delta = Delta, Q_t2 = Q_t2[a], Q_t[a], TDs[[a]], selected_actions2[[a]])
	# 		
	# 		
	# 		# # calculate TD-error
	# 		# if (differential) {
	# 		# 	TD <- r[a] - r_bar[a] + Delta * Q_t2[a] - Q_t[a]
	# 		# } else {
	# 		# 	TD <- r[a] + Delta * Q_t2[a] - Q_t[a]
	# 		# }
	# 		# 
	# 		# # update average reward
	# 		# r_bar[a] <- r_bar[a] + Gamma * TD
	# 		
	# 		
	# 		# update eligbility trace
	# 		z[[a]] <- Delta * Lambda * z[[a]] + (1 - Alpha * Delta * Lambda * sum(z[[a]] * x_t[[a]])) * x_t[[a]]
	# 		
	# 		
	# 		# update w
	# 		if (dutch_traces) {
	# 			
	# 			Q_diff <- Q_t[a] - Q_old[a]
	# 			
	# 			w[[a]] <- w[[a]] +
	# 				Alpha * (TDs[[a]]$Error + Q_diff) * z[[a]] - 
	# 				Alpha * Q_diff * x_t[[a]]
	# 		} else {
	# 			w[[a]] <- w[[a]] + Alpha * TDs[[a]]$Error * x_t[[a]]
	# 		}
	# 		
	# 		
	# 	}
	# 	
	# 	
	# 	# update Q, state set, actions & timer
	# 	Q_old <- Q_t2
	# 	x_t <- x_t2
	# 	s_t <- s_t2
	# 	selected_actions <- selected_actions2
	# 	# selected_actions <- selected_actions2
	# 	
	# 	
	# 	# check for convergence
	# 	
	# 	if (t %% convergence_check_frequency == 0 && t >= convergence_chunk_length) {
	# 		convergence <- detect_pattern(outcomes = outcomes,
	# 												current_t = t - 1,
	# 												chunk_size = convergence_chunk_length,
	# 												cycle_length = convergence_cycle_length)
	# 	}
	# }
	
	print("intervention")
	
	# retrieve profits of last learning episode
	r <- R[[selected_actions$id[1] + (selected_actions$id[2] - 1) * m]]
	
	# record prices and profits of last episode
	outcomes[t,] <- c(selected_actions$value, r)
	
	# move to next time stage
	t <- t + 1
	
	# past actions become today's status
	s_t <- selected_actions$value
	
	### manual intervention
	
	# second agent adheres to learned value approximation (strategy now: full exploitation)
	selected_actions$id[2] <- select_action_id(state_set = s_t, w = w[[2]], epsilon = 0, m = m, feature_specs = feature_specs, available_prices = available_prices)$id
	
	# first agent examines direct rewards and chooses maximum
	R_available <- (selected_actions$id[2]-1) * m + 1:m
	selected_actions$id[1] <- R[R_available] %>% map_dbl(first) %>% which.max()
	
	# get price according to action's id
	selected_actions$value <- available_prices[selected_actions$id]
	selected_actions$greedy <- TRUE
	
	# retrieve deviation profits from list
	r <- R[[selected_actions$id[1] + (selected_actions$id[2]-1) * m]]
	
	# record prices and profits of deviation episode
	outcomes[t,] <- c(selected_actions$value, r)
	
	# update timer
	t <- t + 1
	
	# observe next stage status (i.e. collect price choice id's)
	s_t <- selected_actions$value
	
	### after manual intervention
	while(t <= convergence$convergence_t + TT_intervention) {
		
		# both players fully exploit learned strategy
		selected_actions <- map(.x = w,
												 .f = select_action_id,
												 state_set = s_t,
												 epsilon = 0,
												 m = m,
												 feature_specs = feature_specs,
												 available_prices = available_prices) %>%
			transpose() %>%
			map(unlist)
		
		# get price according to action's id
		# selected_actions <- available_prices[selected_action_ids]
		
		# retrieve profits from list
		r <- R[[selected_actions$id[1] + (selected_actions$id[2]-1) * m]]
		
		# record prices and profits
		outcomes[t,] <- c(selected_actions$value, r)
		
		# observe next stage status (i.e. collect price choice id's)
		s_t <- selected_actions$value
		
		# update timer
		t <- t + 1
		
	}
	
	# truncate from outcomes NA rows 'after convergence'
	outcomes <- na.omit(outcomes)
	
	if(is.na(run_id)) {run_id <- sample.int(1000000, size = 1)}
	print("before return")
	return(list(outcomes = outcomes, w = w, timestamp = Sys.time(),
					available_prices = available_prices, run_id = run_id,
					specs = as.list(match.call()),
					feature_specs = feature_specs,
					convergence = convergence))
}


single_res <- single_run(algorithm = "SARSA",
								 n = 2,
								 seed = 1234567,
								 zeta = 1,
								 rounding_precision = 8,
								 m = 21,
								 TT = 1001,
								 TT_intervention = 2,
								 Alpha = 1*10^-6,
								 Beta = (2 * 10^-2),
								 Gamma = 0.05,
								 Delta = 1,
								 Lambda = 0.5,
								 Epsilon_constant = NA,
								 w_init = 0,
								 r_init = 1.472928,
								 run_id = NA,
								 specifications = list(
								 	degree = 4,
								 	splines_degree = 3,
								 	raw = TRUE,
								 	n_knots = 4,
								 	n_tilings = 10,
								 	n_tiles = 10
								 ),
								 features_by = "poly",								 
								 td_error = "discounted",
								 dutch_traces = TRUE,
								 boltzmann = FALSE,
								 convergence_chunk_length = 1000,
								 convergence_cycle_length = 10,
								 convergence_check_frequency = 1000,
								 c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)


single_simulation_outcomes <- single_res$outcomes

exp(-1 * 10^-5 * c(1, 1000, 10000, 50000, 100000, 150000, 200000, 300000, 500000))

# retrieve number of cores and specify required number of runs
(no_of_cores <- detectCores(all.tests = TRUE, logical = FALSE))
run_ids <- 1:no_of_cores

# run simulations on cluster with specified number of cores
plan(strategy = cluster, workers = no_of_cores)
meta_res <- future_lapply(X = c(0.5, 1., 1.5, 2),
								  FUN = single_run,
								  future.seed = 123456,
								  n = 2,
								  # zeta = 1,
								  rounding_precision = 8,
								  m = 25,
								  TT = 1000000,
								  TT_intervention = 10,
								  Alpha =  1*10^-6,
								  Beta = 1*10^-5,
								  Gamma = 0.05,
								  Delta = 1,
								  Lambda = 0.5,
								  Epsilon_constant = NA,
								  w_init = 0,
								  r_init = 0.2229272,
								  seed = NA,
								  run_id = NA,
								  specifications = list(
								  	degree = 4,
								  	raw = TRUE,
								  	splines_degree = 3,
								  	n_knots = 2,
								  	n_tilings = 10,
								  	n_tiles = 10
								  ),
								  features_by = "poly",
								  td_error = "discounted",
								  dutch_traces = TRUE,
								  boltzmann = FALSE,
								  convergence_chunk_length = 3000,
								  convergence_cycle_length = 10,
								  convergence_check_frequency = 3000,
								  c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)

#str(meta_res)

str(meta_res[[1]])

map(meta_res, .f = ~.$timestamp)
map(meta_res, .f = ~.$w[[1]])
map(meta_res, .f = ~ str(.$specs))


map(meta_res, ~tail(.$outcomes))

source("shiny/visualize_outcomes.R")
shinyApp(ui, server)

map(meta_res, ~.$convergence$convergence_t)

specifications <- list(
	list(raw = TRUE, n_tilings = 1, n_tiles = 15, splines_degree = 3, n_knots = 1),
	list(raw = TRUE, n_tilings = 2, n_tiles = 10, splines_degree = 3, n_knots = 2),
	list(raw = FALSE, n_tilings = 3, n_tiles = 10, splines_degree = 3, n_knots = 3),
	list(raw = FALSE, n_tilings = 4, n_tiles = 10, splines_degree = 3, n_knots = 4)
	)


save(meta_res, file = "simulation_results/8alphas_100000.RData")
meta_res_old <- meta_res


