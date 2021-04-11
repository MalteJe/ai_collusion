library(purrr)
library(tidyquant)
library(tidyverse)
library(parallel)
library(future.apply)
library(nnet)
library(viridis)

getwd() %>%
	list.files(full.names = TRUE) %>%
	str_subset("^(?!.*master.R$)") %>% 
	str_subset("^(?!.*fa.R$)") %>%
	str_subset("^(?!.*po.R$)") %>%
	str_subset("^(?!.*po_normal.R$)") %>%
	str_subset(".R$") %>%
	walk(source)




select_action_po_softmax <- function(state_set, theta, m, available_prices, feature_specs) {
	
	# calculate feature vector for every feasible action
	X <- map(.x = available_prices,
				.f = get_x,
				state_set = state_set,
				feature_specs = feature_specs)
	
	# estimate qualities of state-action combinations
	estimated_values <- map_dbl(.x = X, .f = ~sum(. * theta))
	
	# select action based on estimated values
	enum <- exp(estimated_values)
	denom <- sum(enum)
	prob <- enum/denom
		
	selected_action_id <- sample.int(n = m,
												size = 1,
												prob = prob)
	
	
	# calculate derivate of log of policy (required for update of theta)
	
	adj_X <- map2(.x = X, .y = prob, .f = ~.x * .y)
	log_deriv <- X[[selected_action_id]] - Reduce(`+`, adj_X)
	
	return(list(
		id = selected_action_id,
		value = available_prices[selected_action_id],
		log_deriv = list(log_deriv)))
}






# r <- calculate_profits(c(action$value, 2.4), c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)
# 
# td <- r + Delta 


td_error_po_discounted <- function(r, Delta, s_t, s_t2, w, feature_specs) {
	
	x <- get_x(state_set = s_t, action = NULL, feature_specs = feature_specs)
	v_t <- sum(x * w)
	
	v_t2 <- estimate_state_action_value(state_set = s_t2, action = NULL, feature_specs = feature_specs, w = w)

	error <- r + Delta * v_t2 - v_t
	
	return(list(x_t = x, error = error))
}







single_run_po <- function(n = 2,   # number of players
								  zeta = 0.1, # deviation above monopoly price and below one shot nash price
								  rounding_precision = 6, # rounding available prices after x digits
								  m, # number of discrete prices
								  TT = 1e6, # time periods
								  TT_intervention = 10, # time periods after manual intervention
								  Alpha, # update rule,
								  Gamma, # control speed of update for average reward
								  Delta = 0.95, # discount factor (usually not used in function approximation)
								  Lambda = 0.5, # trace decay rate (dutch traces)
								  r_adjust = 0, # initial 'average reward'
								  seed = NA, # seed for individual run,
								  run_id = NA, # identification of run for apply functions,
								  specifications, # specifications regarding calculation of feature set
								  features_by = "tiling", # calculate features using tiling, polynomial or splines?
								  td_error_method = "discounted",  # use differential reward setting?
								  policy = "softmax",  # logical: use boltzmann to select action probabilistically
								  convergence_chunk_length = 10000, # length of block that is checked against convergence
								  convergence_cycle_length = 10, # what is the maximum cycle length considered
								  convergence_check_frequency = 2000, # how often should convergence be checked
								  ... # further arguments passed to economic environment
) {
	
	print("hey")
	# source(str_c(getwd(), "/selection_methods_and_td.R"))
	
	# workaround to ensure all required functions are loaded on workers
	a <- select_action_po_softmax; a <- td_error_po_discounted
	
	
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
		
		theta_specs <- set_up_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 3)
		w_specs <- set_up_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 2)
		
		theta_length <- specifications$n_tiles^3 * specifications$n_tilings
		w_length <- specifications$n_tiles^2 * specifications$n_tilings
	} else if (features_by == "splines") {
		
		get_x <<- get_x_splines
		# get_x <<- get_x_splines2
		
		theta_specs <- set_up_splines(specifications = specifications, min_price = mc, max_price = max_price, rounding_precision = rounding_precision, vars = 3)
		w_specs <- set_up_splines(specifications = specifications, min_price = mc, max_price = max_price, rounding_precision = rounding_precision, vars = 2)
		
		theta_length <- 3 * (specifications$splines_degree + specifications$n_knots)
		w_length <- 6 * (specifications$splines_degree + specifications$n_knots)
		#length_w <- (specifications$splines_degree + specifications$n_knots)^3
	} else if (features_by == "poly") {
		get_x <<- get_x_poly
		
		theta_specs <- set_up_poly(specifications = specifications, vars = 3)
		w_specs <- set_up_poly(specifications = specifications, vars = 2)
		
		length_states <- n + 1  # '+1' reflects action undertaken
		theta_length <- choose(specifications$degree + length_states, length_states) - 1
		w_length <- choose(specifications$degree + n, n) - 1
	} else if (features_by == "poly_normalized") {
		get_x <<- get_x_poly_normalized
		
		theta_specs <- set_up_poly_normalized(specifications = specifications, min_price = mc, max_price = max_price, vars = 3)
		w_specs <- set_up_poly_normalized(specifications = specifications, min_price = mc, max_price = max_price, vars = 2)
		
		length_states <- n + 1  # '+1' reflects action undertaken
		theta_length <- choose(specifications$degree + length_states, length_states) - 1
		w_length <- choose(specifications$degre + n, n) - 1
	} else if (features_by == "poly_tiling") {
		get_x <<- get_x_poly_tilings
		
		theta_specs <- set_up_poly_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 3)
		w_specs <- set_up_poly_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 2)
		
		length_states <- n + 1  # '+1' reflects action undertaken
		theta_length <- (choose(specifications$degree_poly_tiling + length_states, length_states) - 1) *
			specifications$poly_n_tilings * specifications$poly_n_tiles^length_states
		w_length <- (choose(specifications$degree_poly_tiling + n, n) - 1) *
			specifications$poly_n_tilings * specifications$poly_n_tiles^length_states
		
	} else {
		stop("features_by must be one of 'tiling', 'splines', 'poly', 'poly_normalized' or 'poly_tiling'")
	}
	
	
	# initialize theta and w
	theta_single <- rep(0, theta_length)
	theta <- list(theta_single, theta_single)
	
	w_single <- rep(0, w_length)
	w <- list(w_single, w_single)
	
	# initiate eligibility traces
	z_theta <- theta
	z_w <- w
	
	
	
	# Algorithm ---------------------------------------------------------------
	# 
	# 
	# if(!(Algorithm %in% c("on_policy", "expected", "tree_backup"))) {
	# 	stop("Algorithm must be one of 'on_policy', 'expected' or 'tree_backup'")
	# }
	# 
	# # TD Error ----------------------------------------------------------------
	# 
	# if(!(td_error_method %in% c("differential", "discounted"))) {
	# 	stop("td_error must be 'differential' or 'discounted'")
	# }
	# 
	# td_error <- str_c("td_error", Algorithm, td_error_method, sep = "_") %>% get()
	# 
	# # initialize argument passed continuously to td_error function
	# if (td_error_method == "differential") {
	# 	r_bar <- rep(0, 2)
	# 	TDs <- list(list(Error = 0, r_bar = 0, Gamma = Gamma), list(Error = 0, r_bar = 0, Gamma = Gamma))
	# } else {
	# 	TDs <- list(list(Error = 0), list(Error = 0))
	# }
	# 
	# 
	# 
	# # Policy ------------------------------------------------------------------
	# 
	# if(!(policy %in% c("boltzmann", "greedy"))) {
	# 	stop("policy must be 'greedy' or 'boltzmann'")
	# }
	# 
	
	select_action <- get(str_c("select_action_po", policy, sep = "_"))
	td_error <- get(str_c("td_error_po", td_error_method, sep = "_"))
	
	# pre-determine (time-declining) exploration rate
	# if(is.na(Epsilon_constant)) {
	# 	
	# 	if (policy == "boltzmann") {
	# 		epsilon <- seq(from = 0.1, to = Beta, length.out = TT)
	# 	} else {
	# 		epsilon <- Psi * exp(-Beta * 1:TT)
	# 	}
	# 	
	# } else {
	# 	epsilon <- rep(Epsilon_constant, TT)
	# }
	
	
	
	# Set up trackers ---------------------------------------------------------
	
	t <- 1  # tracks time
	outcomes <- matrix(NA, nrow = TT + TT_intervention + 1, ncol = n * 2,
							 dimnames = list(NULL, map(c("price", "profit"), ~str_c(., 1:n, sep = "_")) %>% unlist()))
	convergence <- list(converged = FALSE)
	
	TDs <- list(list(Error = 0), list(Error = 0))
	
	
	# Sample Initial State ----------------------------------------------------
	
	if (!is.na(seed)) {set.seed(seed)}
	initial_state_ids <- sample(1:m, n, replace = TRUE)
	s_t <- available_prices[initial_state_ids]
	
	
	# execute simulation with learning algorithm ------------------------------
	
	print("before")
	
	
	environment_initialization <- mget(ls())
	
	environment_convergence <- po(passed_environment = environment_initialization)
	
	
	
	
	
	
	# environment_initialization <- mget(ls())
	# 
	# if (Algorithm == "on_policy") {
	# 	environment_convergence <- SARSA(passed_environment = environment_initialization)
	# 	
	# } else if (Algorithm %in% c("expected", "tree_backup")) {
	# 	environment_convergence <- expected_SARSA(passed_environment = environment_initialization)
	# 	
	# }
	# 
	# list2env(x = environment_convergence, envir = environment())
	
	print("intervention")
	
	# Manual Intervention ------------------------------------------------------
	
	
	environment_intervention <- intervention_po(passed_environment = environment_convergence)
	list2env(x = environment_intervention, envir = environment())
	
	
	# Return Results ----------------------------------------------------------
	
	# truncate from outcomes NA rows 'after convergence'
	outcomes <- na.omit(outcomes)
	
	if(is.na(run_id)) {run_id <- sample.int(1000000, size = 1)}
	print("before return")
	return(list(outcomes = outcomes, w = w, timestamp = Sys.time(),
					available_prices = available_prices, run_id = run_id,
					specs = as.list(match.call()),
					theta_specs = theta_specs,
					w_specs = w_specs,
					convergence = convergence,
					get_x = get_x))
}


po <- function(passed_environment) {
	
	list2env(x = passed_environment, envir = environment())
	rm(passed_environment)
	
	
	while (convergence$converged == FALSE && t < TT) {
		
		
		# 		if (t %in% c(25000, 50000, 74500)) { browser()}
		# compute action
		selected_actions <- map(.x = theta,
										.f = select_action,
										state_set = s_t,
										m = m,
										feature_specs = theta_specs,
										available_prices = available_prices) %>%
			transpose() %>% map(unlist, recursive = FALSE)
		
		# selected_actions$id[2] <- m-1
		# selected_actions$value[2] <- available_prices[m-1]
		
		
		# retrieve profits from list
		r <- R[[selected_actions$id[1] + (selected_actions$id[2] - 1) * m]]
		
		# record prices and profits
		outcomes[t,] <- c(selected_actions$value, r)
		
		# adjust profit
		r <- r - r_adjust
		
		
		# # move to next time stage
		t <- t + 1
		
		# past actions become today's status
		s_t2 <- selected_actions$value
		
		
		# if(t %% 200 == 0) {browser()}
		
		# update weights and average reward
		for (a in seq_along(w)) {
			
			
			# calculate TD-error 
			TDs[[a]] <- td_error(r = r[a], Delta = Delta, s_t = s_t, s_t2 = s_t2,
										w = w[[a]], feature_specs = w_specs)
			
			# update traces
			z_w[[a]] <- Lambda * z_w[[a]] + TDs[[a]]$x_t
			z_theta[[a]] <- Lambda * z_theta[[a]] + selected_actions$log_deriv[[a]]
			
			#  update w and theta
			w[[a]] <- w[[a]] + Alpha * TDs[[a]]$error * z_w[[a]]
			theta[[a]] <- theta[[a]] + Alpha * TDs[[a]]$error * z_theta[[a]]
			
		}
		
		
		# update state set
		s_t <- s_t2
		
		
		# check for convergence
		
		if (t %% convergence_check_frequency == 0 && t >= convergence_chunk_length) {
			
			convergence <- detect_pattern(outcomes = outcomes,
													current_t = t - 1,
													chunk_size = convergence_chunk_length,
													cycle_length = convergence_cycle_length)
		}
	}
	
	
	return(mget(ls()))
	
}


intervention_po <- function(passed_environment) {
	
	list2env(x = passed_environment, envir = environment())
	rm(passed_environment)
	
	# second agent adheres to learned value approximation (strategy now: full exploitation)
	selected_actions$id[2] <- select_action(state_set = s_t, theta = theta[[2]], m = m, feature_specs = theta_specs, available_prices = available_prices)$id
	
	# selected_actions$id[2] <- m-1
	# selected_actions$value[2] <- available_prices[m-1]
	
	# first agent examines direct rewards and chooses maximum
	R_available <- (selected_actions$id[2]-1) * m + 1:m
	selected_actions$id[1] <- R[R_available] %>% map_dbl(first) %>% which.max()
	
	# get price according to action's id
	selected_actions$value <- available_prices[selected_actions$id]
	
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
		selected_actions <- map(.x = theta,
										.f = select_action,
										state_set = s_t,
										m = m,
										feature_specs = theta_specs,
										available_prices = available_prices) %>%
			transpose() %>%
			map(unlist)
		
		# retrieve profits from list
		r <- R[[selected_actions$id[1] + (selected_actions$id[2]-1) * m]]
		
		# record prices and profits
		outcomes[t,] <- c(selected_actions$value, r)
		
		# observe next stage status (i.e. collect price choice id's)
		s_t <- selected_actions$value
		
		# update timer
		t <- t + 1
		
	}
	
	
	return(mget(ls()))
	
}



po_res <- single_run_po(n = 2,
				  zeta = 1,
				  rounding_precision = 6,
				  m = 12,
				  TT = 50,
				  TT_intervention = 10,
				  Alpha = 1 * 10^-3,
				  Gamma = NA,
				  Delta = 0.95,
				  Lambda = 0.5,
				  r_adjust = 0.2229272,
				  seed = NA,
				  run_id = 321,
				  specifications = list(
				  	degree = 4,
				  	splines_degree = 3,
				  	n_knots = 4,
				  	degree_sep = 4,
				  	degree_poly_tiling = 4,
				  	poly_n_tilings = 5,
				  	poly_n_tiles = 4,
				  	n_tilings = 1,
				  	n_tiles = 8
				  ),
				  features_by = "poly",
				  td_error_method = "discounted",
				  policy = "softmax",
				  convergence_chunk_length = 20,
				  convergence_cycle_length = 10,
				  convergence_check_frequency = 20,
				  c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)



# single_simulation_outcomes <- single_res$outcomes

0.7 * exp(- 9 * 10^-6 * c(1, 1000, 10000, 50000, 100000, 150000, 200000, 300000, 1000000))

# retrieve number of cores and specify required number of runs
(no_of_cores <- detectCores(all.tests = TRUE, logical = FALSE))
run_ids <- 1:no_of_cores

# run simulations on cluster with specified number of cores
plan(strategy = cluster, workers = no_of_cores)
meta_res <- future_lapply(X = c(0, 0.5, 0.8, 1),
								  FUN = single_run_po,
								  n = 2,
								  future.seed = 123456,
								  zeta = 1,
								  rounding_precision = 6,
								  m = 20,
								  TT = 300000,
								  TT_intervention = 10,
								  Alpha = 1 * 10^-6,
								  Gamma = NA,
								  # Delta = 0.95,
								  Lambda = 0.5,
								  r_adjust = 0.2229272,
								  seed = NA,
								  run_id = NA,
								  specifications = list(
								  	degree = 5,
								  	splines_degree = 3,
								  	n_knots = 4,
								  	degree_poly_tiling = 4,
								  	poly_n_tilings = 5,
								  	poly_n_tiles = 4,
								  	n_tilings = 5,
								  	n_tiles = 8
								  ),
								  features_by = "poly_tiling",
								  td_error_method = "discounted",
								  policy = "softmax",
								  convergence_chunk_length = 2000,
								  convergence_cycle_length = 10,
								  convergence_check_frequency = 2000,
								  c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)



str(meta_res[[1]])

map(meta_res, .f = ~.$timestamp)
map(meta_res, .f = ~.$w[[1]])
map(meta_res, .f = ~ str(.$specs))


map(meta_res, ~tail(.$outcomes, 20))

source("shiny/visualize_outcomes.R")
shinyApp(ui, server)
