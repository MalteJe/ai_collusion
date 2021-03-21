
# function to choose action based on e-greedy policy
select_action_id_greedy <- function(state_set, w, epsilon, m, available_prices, feature_specs) {
	
	# draw from binomial distribution to determine whether to explore or exploit
	if(rbinom(n = 1, size = 1, prob = epsilon)) {
		
		# exploration: choose random action
		selected_action_id <- sample(1:m, 1)
		greedy <- FALSE
		
	} else {
		
		# exploitation: calculate estimated values of available state-action pairs and select maximum
		selected_action_id <- map_dbl(.x = available_prices,
												.f = estimate_state_action_value,
												state_set = state_set,
												feature_specs = feature_specs,
												w = w) %>%
			which.is.max()   #randomly breaks ties
		
		greedy <- TRUE
	}
	
	
	return(list(
		id = selected_action_id,
		value = available_prices[selected_action_id],
		greedy = greedy))
}






select_action_id_boltzmann <- function(state_set, w, epsilon, m, available_prices, feature_specs) {
	estimated_values <- map_dbl(.x = available_prices,
										 .f = estimate_state_action_value,
										 state_set = state_set,
										 feature_specs = feature_specs,
										 w = w)
	
	if(epsilon == 0) {
		res <- which.is.max(estimated_values)
	} else {
		enum <- exp(estimated_values/epsilon)
		denom <- sum(enum)
		prob <- enum/denom
		
		res <- sample.int(n = m,
								size = 1,
								prob = prob)
		
	}
	return(res)
	
}



# TD-Errors ---------------------------------------------------------------


td_error_differential <- function(r, Delta, Q_t2, Q_t, TD, selected_actions) {
	TD$Error <- r - TD$r_bar + Q_t2 - Q_t
	TD$r_bar <-  TD$r_bar + TD$Gamma * TD$Error
	
	return(TD)
}

td_error_discounted <- function(r, Delta, Q_t2, Q_t, TD, selected_actions) {
	TD$Error <- r + Delta * Q_t2 - Q_t
	
	return(TD)
}


# TBD HIER
td_error_expected <- function(r, Delta, Q_t2, Q_t, TD, selected_actions) {
	V_bar <- sum(selected_actions$qualities * selected_actions$probs)
	
	return(r + Delta * V_bar - Q_t)
}
