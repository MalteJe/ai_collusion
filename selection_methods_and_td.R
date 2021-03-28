


# function to choose action based on e-greedy policy
select_action_on_policy_greedy <- function(state_set, w, epsilon, m, available_prices, feature_specs) {
	
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


select_action_on_policy_boltzmann <- function(state_set, w, epsilon, m, available_prices, feature_specs) {
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




# function to choose action based on e-greedy policy (but best action is always computed)
select_action_expected_greedy <- function(state_set, w, epsilon, m, available_prices, feature_specs) {

	# obtain feature vectors for every possible action
	features <- map(.x = available_prices,
						 	  .f = get_x,
							   state_set = state_set,
						 	   feature_specs = feature_specs)
	
	# calculate estimated values of available state-action pairs
	qualities <- map_dbl(.x = features,
								.f = ~sum(. * w))
	
	# derive optimal action(s)
	optimal_actions <- which(qualities == max(qualities))
	
	# draw from binomial distribution to determine whether to explore or exploit
	if(rbinom(n = 1, size = 1, prob = epsilon)) {
		
		# exploration: choose random action
		selected_action_id <- sample(1:m, 1)
		
	} else {
		
		# exploitation: select greedy action
		selected_action_id <- which.is.max(qualities)   #randomly breaks ties
	}
	
	
	# retrieve feature vector and quality of selected state-action-combination (required in update rule)
	x_t <- features[selected_action_id]
	Q_t <- qualities[selected_action_id]
	
	
		# does action reflect the target policy? If yes, calculate rho
	greedy <- (selected_action_id %in% optimal_actions)
	
	if(greedy) {
		le <- length(optimal_actions)
		target_prob <- 1/le
		rho <- (target_prob) / ((1- epsilon)/le + epsilon/m)
	} else {
		rho <- 0L
		target_prob <- 0L
	}
	
	return(list(
		id = selected_action_id,
		value = available_prices[selected_action_id],
		greedy = greedy,
		target_prob = target_prob,
		rho = rho,
		x_t = x_t,
		Q_t = Q_t))
}







# TD-Errors ---------------------------------------------------------------


td_error_on_policy_differential <- function(r, Delta, Q_t2, Q_t, TD) {
	TD$Error <- r - TD$r_bar + Q_t2 - Q_t
	TD$r_bar <-  TD$r_bar + TD$Gamma * TD$Error
	
	return(TD)
}

td_error_on_policy_discounted <- function(r, Delta, Q_t2, Q_t, TD) {
	TD$Error <- r + Delta * Q_t2 - Q_t
	
	return(TD)
}



# Expected SARSA ----------------------------------------------------------

# calculates expected value of upcoming state-action combination by weighting possible actions with their probabilities
calculate_expected_value <- function(r, s_t2, w, epsilon, m, available_prices, feature_specs) {
	
	qualities <- map_dbl(.x = available_prices,
								.f = estimate_state_action_value,
								state_set = s_t2,
								feature_specs = feature_specs,
								w = w)
	
	optimal_actions <- which(qualities == max(qualities))
	
	prob_due_exploration <- epsilon/m
	probs <- rep(prob_due_exploration, m)
	
	
	probs[optimal_actions] <- (1 - epsilon)/length(optimal_actions) + prob_due_exploration
	
	return(sum(qualities * probs))
	
}

td_error_expected_discounted <- function(r, Delta, Q_t, s_t2, w, epsilon, m, available_prices, feature_specs, TD) {
	
	V_bar <- calculate_expected_value(r = r, s_t2 = s_t2, w = w, epsilon = epsilon, m = m, available_prices = available_prices, feature_specs = feature_specs)
	
	TD$Error <- r + Delta * V_bar - Q_t
	
	return(TD)
}


td_error_expected_differential <- function(r, Delta, Q_t, s_t2, w, epsilon, m, available_prices, feature_specs, TD) {
	
	V_bar <- calculate_expected_value(r = r, s_t2 = s_t2, w = w, epsilon = epsilon, m = m, available_prices = available_prices, feature_specs = feature_specs)
	
	TD$Error <- r - TD$r_bar + V_bar - Q_t
	TD$r_bar <- TD$r_bar + TD$Gamma * TD$Error
	
	return(TD)
}


td_error_tree_backup_discounted <- td_error_expected_discounted
td_error_tree_backup_differential <- td_error_expected_differential
select_action_tree_backup_greedy <- select_action_expected_greedy
