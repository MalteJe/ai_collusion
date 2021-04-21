# SARSA -------------------------------------------------------------------


SARSA <- function(passed_environment) {
	
	list2env(x = passed_environment, envir = environment())
	rm(passed_environment)
	
	# compute initial action
	selected_actions <- map(.x = w,
									.f = select_action,
									state_set = s_t,
									epsilon = epsilon[t],
									m = m,
									feature_specs = feature_specs,
									available_prices = available_prices) %>%
		transpose() %>% map(unlist)
	
	# selected_actions$id[2] <- m-1
	# selected_actions$value[2] <- available_prices[m-1]
	
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
		
		# adjust profits
		r <- r - r_adjust
		
		# move to next time stage
		t <- t + 1
		
		# past actions become today's status
		s_t2 <- selected_actions$value
		
		# select actions from all players based on current policies
		selected_actions2 <- map(.x = w,
										 .f = select_action,
										 state_set = s_t2,
										 epsilon = epsilon[t],
										 m = m,
										 feature_specs = feature_specs,
										 available_prices = available_prices) %>%
			transpose() %>%
			map(unlist)
		
		selected_actions2$id[2] <- m-1
		selected_actions2$value[2] <- available_prices[m-1]
		
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
			TDs[[a]] <- td_error(r = r[a], Delta = Delta, Q_t2 = Q_t2[a], Q_t[a], TDs[[a]])
			
			
			# # calculate TD-error
			# if (differential) {
			# 	TD <- r[a] - r_bar[a] + Delta * Q_t2[a] - Q_t[a]
			# } else {
			# 	TD <- r[a] + Delta * Q_t2[a] - Q_t[a]
			# }
			# 
			# # update average reward
			# r_bar[a] <- r_bar[a] + Gamma * TD
			
			
			
			
			if (dutch_traces) {
				
				# update eligbility trace
				z[[a]] <- Delta * Lambda * z[[a]] + (1 - Alpha * Delta * Lambda * sum(z[[a]] * x_t[[a]])) * x_t[[a]]
				
				# update w
				Q_diff <- Q_t[a] - Q_old[a]
				
				w[[a]] <- w[[a]] +
					Alpha * (TDs[[a]]$Error + Q_diff) * z[[a]] - 
					Alpha * Q_diff * x_t[[a]]
			} else {
				# update w
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
	
	# retrieve profits of last learning episode
	r <- R[[selected_actions$id[1] + (selected_actions$id[2] - 1) * m]]
	
	# record prices and profits of last episode
	outcomes[t,] <- c(selected_actions$value, r)
	
	# move to next time stage
	t <- t + 1
	
	# past actions become today's status
	s_t <- selected_actions$value
	
	return(mget(ls()))
	
}



# Expected SARSA ----------------------------------------------------------


expected_SARSA <- function(passed_environment) {
	
	list2env(x = passed_environment, envir = environment())
	rm(passed_environment)


	while (convergence$converged == FALSE && t <= TT) {
		
		
 		# if (t %% 500 == 0) { print(t)}
		# compute action
		selected_actions <- map(.x = w,
										.f = select_action,
										state_set = s_t,
										epsilon = epsilon[t],
										m = m,
										feature_specs = feature_specs,
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
		
		# past actions become today's status
		s_t2 <- selected_actions$value
		
		
		# if(t %% 200 == 0) {browser()}
		
		# update weights and average reward
		for (a in seq_along(w)) {
			
			
			# calculate TD-error 
			TDs[[a]] <- td_error(r = r[a], Delta = Delta, Q_t = selected_actions$Q_t[a], s_t2 = s_t2,
									w = w[[a]], epsilon = epsilon[t], m = m, available_prices = available_prices,
									feature_specs = feature_specs, TD = TDs[[a]])
			
			
			# update eligbility trace
			if (Algorithm == "expected") {
				z[[a]] <- Delta * Lambda * selected_actions$rho[a] * z[[a]] + selected_actions$x_t[[a]]
			} else if (Algorithm == "tree_backup") {
				z[[a]] <- Delta * Lambda * selected_actions$target_prob[a] * z[[a]] + selected_actions$x_t[[a]]
			}
			
			
			
			# update w
			w[[a]] <- w[[a]] + Alpha * TDs[[a]]$Error * z[[a]]
			
		}
		
		
		# update state set
		s_t <- s_t2
		
		
		# check for convergence
		
		if (t %% convergence_check_frequency == 0 && t >= convergence_chunk_length) {
			
			convergence <- detect_pattern(outcomes = outcomes,
													current_t = t,
													chunk_size = convergence_chunk_length,
													cycle_length = convergence_cycle_length)
		}
		
		# # move to next time stage
		t <- t + 1
	}
	
	
	
	
	
	return(mget(ls()))
	
}







# Intervention ------------------------------------------------------------

intervention <- function(passed_environment) {

	list2env(x = passed_environment, envir = environment())
	rm(passed_environment)
	
	# second agent adheres to learned value approximation (strategy now: full exploitation)
	selected_actions$id[2] <- select_action(state_set = s_t, w = w[[2]], epsilon = 0, m = m, feature_specs = feature_specs, available_prices = available_prices)$id
	
	# selected_actions$id[2] <- m-1
	# selected_actions$value[2] <- available_prices[m-1]
	
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
										.f = select_action,
										state_set = s_t,
										epsilon = 0,
										m = m,
										feature_specs = feature_specs,
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
