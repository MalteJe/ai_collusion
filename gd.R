
# define function for a single run (function approximation):

single_run <- function(Algorithm,  # determines type of learning Algorithm
							  n = 2,   # number of players
							  zeta = 0.1, # deviation above monopoly price and below one shot nash price
							  rounding_precision = 6, # rounding available prices after x digits
							  m, # number of discrete prices
							  TT = 1e6, # time periods
							  TT_intervention = 10, # time periods after manual intervention
							  Alpha, # update rule,
							  Beta, # exploration control,
							  Gamma, # control speed of update for average reward
							  Delta = 1, # discount factor (usually not used in function approximation)
							  Lambda = 0, # trace decay rate (dutch traces)
							  Epsilon_constant = NA, # epsilon if exploration is constant
							  Psi = 1, # initial exploration rate at t = 1
							  w_init = 0, # initial weights
							  r_adjust = 0, # initial 'average reward'
							  seed = NA, # seed for individual run,
							  run_id = NA, # identification of run for apply functions,
							  specifications, # specifications regarding calculation of feature set
							  features_by = "tiling", # calculate features using tiling, polynomial or splines?
							  td_error_method = "discounted",  # use differential reward setting?
							  dutch_traces = FALSE, # logical: use eligibility traces?
							  policy = "greedy",  # logical: use boltzmann to select action probabilistically
							  convergence_chunk_length = 10000, # length of block that is checked against convergence
							  convergence_cycle_length = 10, # what is the maximum cycle length considered
							  convergence_check_frequency = 2000, # how often should convergence be checked
							  ... # further arguments passed to economic environment
) {
	
	print(run_id)
	# source(str_c(getwd(), "/selection_methods_and_td.R"))
	
	# workaround to ensure all required functions are loaded on workers
	a <- select_action_on_policy_greedy;a <-  select_action_on_policy_boltzmann;a <-  select_action_expected_greedy; a <-  select_action_tree_backup_greedy; a <-  td_error_on_policy_differential; a <-  td_error_on_policy_discounted; a <- td_error_on_policy_discounted; a <- td_error_expected_discounted; a <-  td_error_expected_differential; a <- td_error_tree_backup_discounted; a <- td_error_tree_backup_differential

	
	
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
		
		feature_specs <- set_up_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 3)
		
		length_w <- specifications$n_tiles^3 * specifications$n_tilings
	} else if (features_by == "tabular") {
		
		get_x <<- get_x_tiling
		
		specifications$n_tilings <- 1
		specifications$n_tiles <- m
		
		feature_specs <- set_up_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 3)
		
		length_w <- specifications$n_tiles^3 * specifications$n_tilings
	} else if (features_by == "splines") {
		
		get_x <<- get_x_splines
		# get_x <<- get_x_splines2
		
		feature_specs <- set_up_splines(specifications = specifications, min_price = mc, max_price = max_price, rounding_precision = rounding_precision, vars = 3)
		
		length_w <- 6 * (specifications$splines_degree + specifications$n_knots)
		#length_w <- (specifications$splines_degree + specifications$n_knots)^3
	} else if (features_by == "poly") {
		get_x <<- get_x_poly
		
		feature_specs <- set_up_poly(specifications = specifications, vars = 3)
		
		length_states <- n + 1  # '+1' reflects action undertaken
		length_w <- choose(specifications$degree + length_states, length_states) - 1
	} else if (features_by == "poly_normalized") {
		get_x <<- get_x_poly_normalized
		
		feature_specs <- set_up_poly_normalized(specifications = specifications, min_price = mc, max_price = max_price, vars = 3)
		
		length_states <- n + 1  # '+1' reflects action undertaken
		length_w <- choose(specifications$degree + length_states, length_states) - 1
	} else if (features_by == "poly_separated"){
		get_x <<- get_x_poly_separate
		
		feature_specs <- set_up_poly_separate(specifications = specifications, available_prices, vars = 2)
		length_w <- (choose(specifications$degree_sep + n, n) - 1) * m
	} else if (features_by == "poly_tiling") {
		get_x <<- get_x_poly_tilings
		
		feature_specs <- set_up_poly_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 3)
		
		length_states <- n + 1  # '+1' reflects action undertaken
		length_w <- (choose(specifications$degree_poly_tiling + length_states, length_states) - 1) *
			specifications$poly_n_tilings * specifications$poly_n_tiles^length_states
		
	} else {
		stop("features_by must be one of 'tiling', 'splines', 'poly' 'poly_normalized', `poly_separated` or 'poly_tiling'")
	}
	
	
	w_single <- rep(w_init, length_w)
	w <- list(w_single, w_single)
	
	# initiate eligibility trace
	z_single <- rep(0, length_w)
	z <- list(z_single, z_single)
	
	
	
# Algorithm ---------------------------------------------------------------
	
	
	if(!(Algorithm %in% c("on_policy", "expected", "tree_backup"))) {
		stop("Algorithm must be one of 'on_policy', 'expected' or 'tree_backup'")
	}

# TD Error ----------------------------------------------------------------

	if(!(td_error_method %in% c("differential", "discounted"))) {
		stop("td_error must be 'differential' or 'discounted'")
	}
	
	td_error <- str_c("td_error", Algorithm, td_error_method, sep = "_") %>% get()
	
	# initialize argument passed continuously to td_error function
	if (td_error_method == "differential") {
		r_bar <- rep(0, 2)
		TDs <- list(list(Error = 0, r_bar = 0, Gamma = Gamma), list(Error = 0, r_bar = 0, Gamma = Gamma))
	} else {
		TDs <- list(list(Error = 0), list(Error = 0))
	}
	
	

# Policy ------------------------------------------------------------------

	if(!(policy %in% c("boltzmann", "greedy"))) {
		stop("policy must be 'greedy' or 'boltzmann'")
	}

	
	select_action <- get(str_c("select_action", Algorithm, policy, sep = "_"))
	
	# pre-determine (time-declining) exploration rate
	if(is.na(Epsilon_constant)) {
		
		if (policy == "boltzmann") {
			epsilon <- seq(from = 0.1, to = Beta, length.out = TT)
		} else {
			epsilon <- Psi * exp(-Beta * 1:TT)
		}
		
	} else {
		epsilon <- rep(Epsilon_constant, TT)
	}
	
	

# Set up trackers ---------------------------------------------------------

	t <- 1  # tracks time
	outcomes <- matrix(NA, nrow = TT + TT_intervention + 1, ncol = n * 2,
							 dimnames = list(NULL, map(c("price", "profit"), ~str_c(., 1:n, sep = "_")) %>% unlist()))
	convergence <- list(converged = FALSE)
	
	

# Sample Initial State ----------------------------------------------------

	if (!is.na(seed)) {set.seed(seed)}
	initial_state_ids <- sample(1:m, n, replace = TRUE)
	s_t <- available_prices[initial_state_ids]
	

# execute simulation with learning algorithm ------------------------------
	
	# print("before")
	
	environment_initialization <- mget(ls())
	
	if (Algorithm == "on_policy") {
		environment_convergence <- SARSA(passed_environment = environment_initialization)
		
	} else if (Algorithm %in% c("expected", "tree_backup")) {
		environment_convergence <- expected_SARSA(passed_environment = environment_initialization)
		
	}
	
	list2env(x = environment_convergence, envir = environment())
	
	# print("intervention")

# Manual Intervention ------------------------------------------------------

	environment_intervention <- intervention(passed_environment = environment_convergence)
	list2env(x = environment_intervention, envir = environment())


# Return Results ----------------------------------------------------------
	
	# truncate from outcomes NA rows 'after convergence'
	outcomes <- na.omit(outcomes)
	
	if(is.na(run_id)) {run_id <- sample.int(1000000, size = 1)}
	# print("before return")
	return(list(outcomes = outcomes, w = w, timestamp = Sys.time(),
					available_prices = available_prices, run_id = run_id,
					specs = as.list(match.call()),
					feature_specs = feature_specs,
					convergence = convergence,
					get_x = get_x))
}
