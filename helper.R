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
