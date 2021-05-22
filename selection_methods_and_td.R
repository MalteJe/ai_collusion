# Expected SARSA ----------------------------------------------------------


# function to choose action based on e-greedy policy (best action is always computed)
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
	
	
	# does action reflect the target policy? If yes, calculate imortance sampling ratio 'rho'
	greedy <- (selected_action_id %in% optimal_actions)
	
	# retrieve probability of selecting the action under target policy and calculate rho
	if(greedy) {
		le <- length(optimal_actions)                              # number of 'optimal' actions (usually 1, unless at the very beginning of the simulation)
		target_prob <- 1/le                                        # probability of choosing greedy action under target policy
		rho <- target_prob / ((1- epsilon)/le + epsilon/m)       # importance sampling ratio (second term is probability under behavior policy)
	} else {
		target_prob <- 0L
		rho <- 0L
	}
	
	# return results
	return(list(
		id = selected_action_id,
		value = available_prices[selected_action_id],
		greedy = greedy,
		target_prob = target_prob,
		rho = rho,
		x_t = x_t,
		Q_t = Q_t))
}


# TD-Errors


# calculates expected value of upcoming state-action combination by weighting possible actions with their probabilities.
calculate_expected_value <- function(r, s_t2, w, epsilon, m, available_prices, feature_specs) {
	
	# calculate estimated qualities of all actions in upcoming episode
	qualities <- map_dbl(.x = available_prices,
								.f = estimate_state_action_value,
								state_set = s_t2,
								feature_specs = feature_specs,
								w = w)
	
	# determine optimal actions
	optimal_actions <- which(qualities == max(qualities))
	
	# initialize probabilities of all actions due to exploration
	prob_due_exploration <- epsilon/m
	probs <- rep(prob_due_exploration, m)
	
	# adjust probabilities of optimal actions to account for exploitation
	probs[optimal_actions] <- (1 - epsilon)/length(optimal_actions) + prob_due_exploration
	
	# return probability-weighted (i.e. Expected) value of next state
	return(sum(qualities * probs))
	
}


# td_error_expected_discounted calculates the TD error utilized in the expected SARSA algorithm
td_error_expected_discounted <- function(r, Delta, Q_t, s_t2, w, epsilon, m, available_prices, feature_specs, TD) {
	
	# calculate expected value of upcoming state (irrespective of actually chosen action in upcoming period)
	V_bar <- calculate_expected_value(r = r, s_t2 = s_t2, w = w, epsilon = epsilon, m = m, available_prices = available_prices, feature_specs = feature_specs)
	
	# calculate and return discounted TD error 
	TD$Error <- r + Delta * V_bar - Q_t
	
	return(TD)
}


td_error_expected_differential <- function(r, Delta, Q_t, s_t2, w, epsilon, m, available_prices, feature_specs, TD) {
	
	V_bar <- calculate_expected_value(r = r, s_t2 = s_t2, w = w, epsilon = epsilon, m = m, available_prices = available_prices, feature_specs = feature_specs)
	
	TD$Error <- r - TD$r_bar + V_bar - Q_t
	TD$r_bar <- TD$r_bar + TD$Gamma * TD$Error
	
	return(TD)
}

# Select action in prolonged deviation --------------------------------------

# select_action_deviation handles the action selection of the cheating player during the prolonged deviation
select_action_deviation <- function(selected_action_id, state_set, w, available_prices, feature_specs) {
	
	# obtain feature vectors for every possible action (still required to determine whether deviation coincides with learned strategy, i.e. is 'on path' or 'off-path')
	features <- map(.x = available_prices,
						 .f = get_x,
						 state_set = state_set,
						 feature_specs = feature_specs)
	
	# calculate estimated values of available state-action pairs
	qualities <- map_dbl(.x = features,
								.f = ~sum(. * w))
	
	# derive optimal action(s)
	optimal_actions <- which(qualities == max(qualities))
	
	
	# retrieve feature vector and quality of selected state-action-combination (required in update rule)
	x_t <- features[selected_action_id]
	Q_t <- qualities[selected_action_id]
	
	
	# does action reflect the target policy? If yes, calculate imortance sampling ratio 'rho'
	greedy <- (selected_action_id %in% optimal_actions)
	
	# retrieve probability of selecting the action under target policy and calculate rho
	if(greedy) {
		target_prob <- 1/length(optimal_actions)                   # probability of choosing greedy action under target policy
		rho <- 1L                                                  # importance sampling ratio (if deviation coincides with optimal action, target policy is equal to behavior policy, i.e. 1)
	} else {
		target_prob <- 0L
		rho <- 0L
	}
	
	# return results
	return(list(
		id = selected_action_id,
		value = available_prices[selected_action_id],
		greedy = greedy,
		target_prob = target_prob,
		rho = rho,
		x_t = x_t,
		Q_t = Q_t))
}




# Tree backup -------------------------------------------------------------

# tree backup logic of action selected and TD error estimation is equivalent to expected SARSA 
td_error_tree_backup_discounted <- td_error_expected_discounted
td_error_tree_backup_differential <- td_error_expected_differential
select_action_tree_backup_greedy <- select_action_expected_greedy


# On-Policy ---------------------------------------------------------------

# function to choose action based on e-greedy policy in on-policy learning algorithm
select_action_on_policy_greedy <- function(state_set, w, epsilon, m, available_prices, feature_specs) {
	
	# draw from binomial distribution to determine whether to explore or exploit
	if(rbinom(n = 1, size = 1, prob = epsilon)) {
		
		# exploration: choose random action
		selected_action_id <- sample(1:m, 1)
		selected_action <- available_prices[selected_action_id]
		
		# retrieve feature vector and quality of randomly selected action (required in update rule)
		x_t <- list(get_x(state_set = state_set, action = selected_action, feature_specs = feature_specs))
		Q_t <- sum(x_t[[1]] * w)
		
	} else {
		
		# exploitation: calculate estimated values of available state-action pairs and select maximum
		features <- map(.x = available_prices,
							 .f = get_x,
							 state_set = state_set,
							 feature_specs = feature_specs)
		
		# calculate estimated values of available state-action pairs
		qualities <- map_dbl(.x = features,
									.f = ~sum(. * w))
		
		# derive optimal action(s)
		selected_action_id <- which.is.max(qualities)
		selected_action <- available_prices[selected_action_id]
		
		# retrieve feature vector and quality of selected state-action-combination (required in update rule)
		x_t <- features[selected_action_id]
		Q_t <- qualities[selected_action_id]
	}
	
	# return results as list
	return(list(
		id = selected_action_id,
		value = selected_action,
		x_t = x_t,
		Q_t = Q_t))
}


# TD-Errors

# td_error_on_policy_discounted calculates the TD error utilized in the on-line SARSA algorithm
td_error_on_policy_discounted <- function(r, Delta, Q_t2, Q_t, TD) {
	
	TD$Error <- r + Delta * Q_t2 - Q_t
	
	return(TD)
}