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


optimize_grid <- function(w, price_grid, available_prices, feature_specs, get_x_run) {
	
	get_x <<- get_x_run
	
	map_dbl(.x = price_grid,
			  .f = optimize_action,
			  available_prices = available_prices,
			  feature_specs = feature_specs, 
			  w = w)
}





# shifts positions of a vector by x steps
shifter <- function(n = 1, x) {
	if (n == 0) x else c(tail(x, -n), head(x, n))
}

# maps action id to state id
get_state_id <- function(action_ids, m) {   # needs to be adjusted for n > 3!!!
	(action_ids[1] - 1) * m + action_ids[2]
}



# convergence rules -------------------------------------------------------

# is_recurring checks whether a chunk of outcomes strictly follows a cycle of given length
is_recurring <- function(cycle_length, chunk) {
	
	# every column is split into separate vectors representing a particular cycle position, 'uneven split' warnings are suppressed
	suppressWarnings(apply(X = chunk, MARGIN = 2, FUN = split, f = 1:cycle_length)) %>%
		unlist(recursive = FALSE) %>%
		
		# checks whether all elements in dedicated vector are equal
		map(.f = unique) %>%
		map_lgl(.f = ~length(.) == 1) %>%
		all()
}


detect_pattern <- function(outcomes, current_t, chunk_size, cycle_length) {
	
	# extract price columns of the considered time period to check for convergence
	chunk <- outcomes[((current_t-chunk_size + 1):current_t),1:2]
	
	# checks for cycles of all provided lengths
	patterns_by_cycle_length <- map_lgl(.x = 1:cycle_length,
													.f = is_recurring,
													chunk = chunk)
	
	# if any pattern could be detected, return cycle length and logical indicating convergence
	if (any(patterns_by_cycle_length)) {
		cycle_length <- which(patterns_by_cycle_length, TRUE) %>%
			min()
		
		return(list(converged = TRUE, cycle_length = cycle_length, convergence_t = current_t + 1))
	} else {
		return(list(converged = FALSE, cycle_length = NA, convergence_t = current_t + 1))
	}
}






# Experiment Helpers ------------------------------------------------------






experiment_alpha <- function(alpha, features_by, runs, m, TT) {
	
	print(str_c("starting experiment. Features_by = ", features_by, "    | Alpha = ", alpha))
	
	# retrieve number of cores and specify requested number of runs
	(no_of_cores <- detectCores(all.tests = TRUE, logical = FALSE))
	plan(strategy = cluster, workers = no_of_cores)   #THIS MAY BE ADJUSTED
	
	future_lapply(X = 1:runs,
					  FUN = single_run,
					  future.seed = 123456,
					  Algorithm = "expected",
					  n = 2,
					  zeta = 1,
					  rounding_precision = 8,
					  m = m,
					  TT = TT,
					  TT_intervention = 10,
					  Alpha =  alpha,
					  Beta = 1*10^-4,
					  Gamma = 0.05,
					  Delta = 0.95,
					  Lambda = 0.3,
					  Epsilon_constant = NA,
					  Psi = 0.7,
					  w_init = 0,
					  r_adjust = 0.2229272,
					  seed = NA,
					  features_by = features_by,
					  specifications = list(
					  	degree_sep = 4,
					  	degree_poly_tiling = 4,
					  	poly_n_tilings = 4,
					  	poly_n_tiles = 3,
					  	n_tilings = 5,
					  	n_tiles = 5
					  ),
					  td_error_method = "discounted",
					  dutch_traces = TRUE,
					  policy = "greedy",
					  convergence_chunk_length = 1000,
					  convergence_cycle_length = 10,
					  convergence_check_frequency = 1000,
					  c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)
}


vary_alpha <- function(experiment_specs, runs, m, TT) {
	map(.x = experiment_specs$alphas,
		 .f = experiment_alpha,
		 features_by = experiment_specs$features,
		 runs = runs,
		 m = m,
		 TT = TT)
}



# temp <- vary_alpha(
# 	experiment_specs = list(
# 		features = "tabular",
# 		alphas = alphas
# 		),
# 	runs = 4,
# 	m = 11)
