select_action_nd <- function(x_t, theta, theta_subsets, sigma_fix) {
	
	# adjustfeature vector
	x_t <- c(x_t, TRUE)  # 1 is the intercept for the adjustments
	
	# calculate mu and sigma based on state features
	mu <- sum(theta[theta_subsets[[1]]] * x_t)
	
	elements <- theta[theta_subsets[[2]]] * x_t
	
	expo <- sum(elements)
	sigma <- exp(expo)
	
	#sigma <- exp(sum(theta[theta_subsets[[2]]] * x_t))
	
	if (is.na(sigma)) {
		
		print("sigma is NA")
		print(sigma)
		print(expo)
	} else if(sigma == 0) {
		
		print("sigma is exactly 0")
		print(sigma)
		print(expo)
	}
	 
	
	# draw action from normal distribution
	action <- max(rnorm(n = 1, mean = mu, sd = sigma), 1)
	
	# calculate log-derivatives w.r.t. theta
	mu_deriv <- 1/sigma^2 * (action - mu) * x_t
	sigma_deriv <- (((action - mu)^2)/sigma^2 - 1) * x_t
	
	return(list(
		value = action,
		log_deriv = list(c(mu_deriv, sigma_deriv))))
}



select_action_nd_simple <- function(x_t, theta, theta_subsets, sigma_fix) {
	
	# adjustfeature vector
	x_t <- c(x_t, TRUE)  # 1 is the intercept for the adjustments
	
	# calculate mu and sigma based on state features
	mu <- sum(theta * x_t)
	
	# draw action from normal distribution
	action <- max(rnorm(n = 1, mean = mu, sd = sigma_fix), 1)
	
	# calculate log-derivatives w.r.t. theta
	mu_deriv <- x_t/sigma_fix^2 * (action - mu)
	
	
	return(list(
		value = action,
		log_deriv = list(mu_deriv)))
}





td_error_nd <- function(r, Delta, x_t, x_t2, w) {
	
	v_t <- sum(x_t * w)
	v_t2 <- sum(x_t2 * w)
	
	return(r + Delta * v_t2 - v_t)
}







single_run_nd <- function(n = 2,   # number of players
								  zeta = 0.1, # deviation above monopoly price and below one shot nash price
								  rounding_precision = 6, # rounding available prices after x digits
								  mu_adjust = 1.472928,
								  sigma_adjust,
								  sigma_control = NA,
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
								  # policy = "softmax",  # logical: use boltzmann to select action probabilistically
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
	
	if (features_by == "tiling") {
		get_x <<- get_x_tiling
		
		feature_specs <- set_up_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 2)
		
		feature_length <- specifications$n_tiles^2 * specifications$n_tilings
		
		
	} else if (features_by == "splines") {

		get_x <<- get_x_splines
		# get_x <<- get_x_splines2

		feature_specs <- set_up_splines(specifications = specifications, min_price = mc, max_price = max_price, rounding_precision = rounding_precision, vars = 2)
		feature_length <- 3 * (specifications$splines_degree + specifications$n_knots)
		
	} else if (features_by == "poly") {
		get_x <<- get_x_poly

		feature_specs <- set_up_poly(specifications = specifications, vars = 2)
		feature_length <- choose(specifications$degree + n, n) - 1
		
	} else if (features_by == "poly_normalized") {
		get_x <<- get_x_poly_normalized

		feature_specs <- set_up_poly_normalized(specifications = specifications, min_price = mc, max_price = max_price, vars = 2)
		feature_length <- choose(specifications$degre + n, n) - 1
		
	} else if (features_by == "poly_tiling") {
		get_x <<- get_x_poly_tilings

		feature_specs <- set_up_poly_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 2)
		feature_length <- (choose(specifications$degree_poly_tiling + n, n) - 1) *
			specifications$poly_n_tilings * specifications$poly_n_tiles^n
		
	} else {
		stop("features_by must be one of 'tiling', 'splines', 'poly', 'poly_normalized' or 'poly_tiling'")
	}
	
	# initialize w
	w_single <- rep(0, feature_length)
	w <- list(w_single, w_single)
	
	# initialize theta
	theta_mu_single <- c(w_single, mu_adjust)
	
	# sigma
	if (is.na(sigma_control)) {   # i.e. sigma is part of the optimization problem
	
		
		theta_sigma_single <- c(w_single, sigma_adjust)
		theta_single <- c(theta_mu_single, theta_sigma_single)
		
		half_length <- length(theta_mu_single)
		theta_length <- 2 * half_length
		
		# theta subsets to be retrieved continuously for calculating log derivative
		theta_subsets <- list(
			1:half_length,
			(half_length+1):(theta_length)
		)
		
		select_action <- select_action_nd
		
	} else {                    # i.e. sigma is not part of the optimization problem
		
		theta_single <- theta_mu_single
		
		theta_length <- length(theta_mu_single)
		
		theta_subsets <- NA
		
		select_action <- select_action_nd_simple
		
		# fixed or time-declining sigma
		if (sigma_control > 0) {
			sigma <- rep(sigma_control, TT + TT_intervention)
		} else {
			sigma <- exp(sigma_adjust) * exp(sigma_control * 1:(TT + TT_intervention))
		}
	}
	
	theta <- list(theta_single, theta_single)
	
	

	# initiate eligibility traces
	z_w <- w
	z_theta_single <- rep(0, theta_length)
	z_theta <- list(z_theta_single, z_theta_single)
	
	
	# Set up trackers ---------------------------------------------------------
	
	t <- 1  # tracks time
	outcomes <- matrix(NA, nrow = TT + TT_intervention + 1, ncol = n * 2,
							 dimnames = list(NULL, map(c("price", "profit"), ~str_c(., 1:n, sep = "_")) %>% unlist()))
	convergence <- list(converged = FALSE)
	
	
	# Sample Initial State ----------------------------------------------------
	
	if (!is.na(seed)) {set.seed(seed)}
	#initial_state_ids <- sample(1:m, n, replace = TRUE)
	s_t <- runif(n = 2, min = mc, max = max_price)
	
	x_t <- get_x(state_set = s_t, action = NULL, feature_specs = feature_specs)
	
	
	# execute simulation with learning algorithm ------------------------------
	
	print("before")
	
	while (convergence$converged == FALSE && t < TT) {
		
		# 	if (t %in% c(25000, 50000, 74500)) { browser()}
		

		
		# compute action
		selected_actions <- map(.x = theta,
										.f = select_action,
										theta_subsets = theta_subsets,
										sigma_fix = sigma[t],
										x_t = x_t) %>%
			transpose() %>% map(unlist, recursive = FALSE)
		
		# selected_actions$value[2] <- 1.97
		
		# calculate profits
		r <- calculate_profits(p = selected_actions$value, ...)
		
		# record prices and profits
		outcomes[t,] <- c(selected_actions$value, r)
		
		# normalize profits (= reward)
		r <- r - r_adjust
		
		# if(t %% 1 == 0) {
		# 	print(t)
		# 	print(selected_actions$value)
		# 	print(r)
		# }
		
		
		# # move to next time stage
		t <- t + 1
		
		# past actions become today's status
		s_t2 <- selected_actions$value
		x_t2 <- get_x(state_set = s_t2, action = NULL, feature_specs = feature_specs)
		
		# if(t %% 200 == 0) {browser()}
		

		
		# update weights and average reward
		for (a in seq_along(w)) {
			
			
			# calculate TD-error 
			error_t <- td_error_nd(r = r[a], Delta = Delta, x_t = x_t, x_t2 = x_t2, w = w[[a]])
			
			# update traces
			z_w[[a]] <- Lambda * z_w[[a]] + x_t
			z_theta[[a]] <- Lambda * z_theta[[a]] + selected_actions$log_deriv[[a]]
			
			#  update w and theta
			w[[a]] <- w[[a]] + Alpha * error_t * z_w[[a]]
			theta[[a]] <- theta[[a]] + Alpha * error_t * z_theta[[a]]
			
			# print(error_t)
		}
		
		
		# if(t %% 1 == 0) {
		# 	print(z_theta[[1]][1100:1121])
		# 	print(theta[[1]][1100:1121])
		# }
		# 
		# update state set
		s_t <- s_t2
		x_t <- x_t2
		
		
		# check for convergence
		
		# if (t %% convergence_check_frequency == 0 && t >= convergence_chunk_length) {
		# 	
		# 	convergence <- detect_pattern(outcomes = outcomes,
		# 											current_t = t - 1,
		# 											chunk_size = convergence_chunk_length,
		# 											cycle_length = convergence_cycle_length)
		# }
	}
	
	print(str_c("intervention at ", t))
	
	# Manual Intervention ------------------------------------------------------
	
	
	# second agent adheres to learned value approximation
	
	selected_actions$value[2] <- select_action(x_t = x_t, theta = theta[[2]], theta_subsets = theta_subsets, sigma_fix = sigma[t])$value
	# selected_actions$value[2] <- 1.97
	
	# first agent plays best response
	selected_actions$value[1] <- best_response(p_ = selected_actions$value[2], n = n, ...)
	
	# retrieve deviation profits
	r <- calculate_profits(p = selected_actions$value, ...)
	
	# record prices and profits of deviation episode
	outcomes[t,] <- c(selected_actions$value, r)
	
	# update timer
	t <- t + 1
	
	# observe next stage status (i.e. collect price choices)
	s_t <- selected_actions$value
	
	x_t <- get_x(state_set = s_t, action = NULL, feature_specs = feature_specs)
	
	### after manual intervention
	while(t < TT+ TT_intervention) {
		
		# both players fully exploit learned strategy
		selected_actions <- map(.x = theta,
										.f = select_action,
										x_t = x_t,
										theta_subsets = theta_subsets,
										sigma_fix = sigma[t]) %>%
			transpose() %>%
			map(unlist)
		
		# retrieve profits from list
		r <- calculate_profits(p = selected_actions$value,  ...)
		
		# record prices and profits
		outcomes[t,] <- c(selected_actions$value, r)
		
		# observe next stage status (i.e. collect price choices)
		s_t <- selected_actions$value
		
		x_t <- get_x(state_set = s_t, action = NULL, feature_specs = feature_specs)
		
		# update timer
		t <- t + 1
		
	}
	
	
	# Return Results ----------------------------------------------------------
	
	# truncate from outcomes NA rows 'after convergence'
	outcomes <- na.omit(outcomes)
	
	if(is.na(run_id)) {run_id <- sample.int(1000000, size = 1)}
	print("before return")
	return(list(outcomes = outcomes, w = w, timestamp = Sys.time(),
					run_id = run_id,
					specs = as.list(match.call()),
					feature_specs = feature_specs,
					convergence = list(convergence_t = TT),
					get_x = get_x))
}