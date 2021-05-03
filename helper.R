# function approximation --------------------------------------------------------------

# estimate_state_action_value returns the estimated value of playing some particular action given the state and coefficients/weights w vector
estimate_state_action_value <- function(state_set, action, feature_specs, w) {
	x <- get_x(state_set = state_set, action = action, feature_specs = feature_specs)
	sum(x * w) # surprisingly, benchmarking suggested that sum(x*w) tends to be faster than crossprod(x, w)
}


# convergence rules -------------------------------------------------------

# is_recurring checks whether a chunk of outcomes strictly follows a cycle of given length
is_recurring <- function(cycle_length, chunk) {

	# every column is split into separate vectors representing a particular cycle position, 'uneven split' warnings are suppressed
	suppressWarnings(apply(X = chunk, MARGIN = 2, FUN = split, f = 1L:cycle_length)) %>%
		unlist(recursive = FALSE) %>%
		
		# checks whether ALL elements in vector (with particular cycle position) are equal. If so, TRUE is returned
		map(.f = unique) %>%
		map_lgl(.f = ~length(.) == 1L) %>%
		all()
}

# detect pattern checks whether the algorithms have converged in the sense that outcomes are stable
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
		
		return(list(converged = TRUE, cycle_length = cycle_length, convergence_t = current_t))
	} else {
		return(list(converged = FALSE, cycle_length = NA, convergence_t = current_t))
	}
}
