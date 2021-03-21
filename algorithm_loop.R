# SARSA -------------------------------------------------------------------


SARSA <- function(passed_environment) {
	
	browser()
	
	list2env(x = passed_environment, envir = environment())
	rm(passed_environment)
	
	
	# compute initial action
	selected_actions <- map(.x = w,
									.f = select_action_id,
									state_set = s_t,
									epsilon = epsilon[t],
									m = m,
									feature_specs = feature_specs,
									available_prices = available_prices) %>%
		transpose() %>% map(unlist)
	
	# selected_actions$value <- available_prices[selected_actions$id]
	
	# extract features from current state-action combination
	x_t <- map(.x = selected_actions$value,
				  .f = get_x,
				  state_set = s_t,
				  feature_specs = feature_specs)
	
	# initialize state-action-value
	Q_old <- rep(0, n)
	
	
	while (convergence$converged == FALSE && t < TT) {
		
		# retrieve profits from list
		r <- R[[selected_actions$id[1] + (selected_actions$id[2] - 1) * m]]
		
		# record prices and profits
		outcomes[t,] <- c(selected_actions$value, r)
		
		# move to next time stage
		t <- t + 1
		
		# past actions become today's status
		s_t2 <- selected_actions$value
		
		# select actions from all players based on current policies
		selected_actions2 <- map(.x = w,
										 .f = select_action_id,
										 state_set = s_t2,
										 epsilon = epsilon[t],
										 m = m,
										 feature_specs = feature_specs,
										 available_prices = available_prices) %>%
			transpose() %>%
			map(unlist)
		# selected_actions2$id[2] <- m - 3
		
		# selected_actions2 <- available_prices[selected_action_ids2]
		
		# extract features from current state-action combination
		x_t2 <- map(.x = selected_actions2$value,
						.f = get_x,
						state_set = s_t2,
						feature_specs = feature_specs)
		
		
		# calculate "quality" of selected actions given states
		
		#previous episode
		Q_t <- map2_dbl(.x = x_t,
							 .y = w,
							 .f = ~sum(.x * .y))
		
		# current episode
		Q_t2 <- map2_dbl(.x = x_t2,
							  .y = w,
							  .f = ~sum(.x * .y))
		
		
		# if(t %% 200 == 0) {browser()}
		
		# update weights and average reward
		for (a in seq_along(w)) {
			
			
			# calculate TD-error 
			TDs[[a]] <- td_error(r = r[a], Delta = Delta, Q_t2 = Q_t2[a], Q_t[a], TDs[[a]], selected_actions2[[a]])
			
			
			# # calculate TD-error
			# if (differential) {
			# 	TD <- r[a] - r_bar[a] + Delta * Q_t2[a] - Q_t[a]
			# } else {
			# 	TD <- r[a] + Delta * Q_t2[a] - Q_t[a]
			# }
			# 
			# # update average reward
			# r_bar[a] <- r_bar[a] + Gamma * TD
			
			
			# update eligbility trace
			z[[a]] <- Delta * Lambda * z[[a]] + (1 - Alpha * Delta * Lambda * sum(z[[a]] * x_t[[a]])) * x_t[[a]]
			
			
			# update w
			if (dutch_traces) {
				
				Q_diff <- Q_t[a] - Q_old[a]
				
				w[[a]] <- w[[a]] +
					Alpha * (TDs[[a]]$Error + Q_diff) * z[[a]] - 
					Alpha * Q_diff * x_t[[a]]
			} else {
				w[[a]] <- w[[a]] + Alpha * TDs[[a]]$Error * x_t[[a]]
			}
			
			
		}
		
		
		# update Q, state set, actions & timer
		Q_old <- Q_t2
		x_t <- x_t2
		s_t <- s_t2
		selected_actions <- selected_actions2
		# selected_actions <- selected_actions2
		
		
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




expected_SARSA <- function(passed_environment) {
	
	browser()
	
	list2env(x = passed_environment, envir = environment())
	rm(passed_environment)
	
	
	
	
	return(mget(ls()))
	
}
