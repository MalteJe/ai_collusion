
# define function for a single run (function approximation):

single_run <- function(Algorithm,  # determines type of learning Algorithm
							  # n = 2,   # number of players
							  zeta = 0.1, # deviation above monopoly price and below one shot nash price
							  rounding_precision = 8, # rounding available prices after x digits
							  m, # number of discrete prices
							  TT = 1e6, # time periods
							  TT_intervention = 10, # time periods after manual intervention
							  Alpha, # update rule,
							  Beta, # exploration control,
							  Gamma, # control speed of update for average reward, ignored if td_error_method = 'discounted'
							  Delta = 1, # discount factor
							  Lambda = 0, # trace decay rate (dutch traces)
							  Epsilon_constant = NA, # epsilon if exploration is constant
							  Psi = 1, # initial exploration rate at t = 1
							  w_init = 0, # initial weights
							  r_adjust = 0, # if td_error_method = 'discounted': profit adjustment, if td_error_method = differential: initial 'average reward'
							  seed = NA, # seed for individual run,
							  run_id = NA, # identification of run, iterated over in experiment
							  specifications, # list with specifications regarding calculation of feature set, values depend on chosen feature extraction method
							  features_by = "tiling", # specify feature extraction method
							  td_error_method = "discounted",  # use discounted or differential setting
							  # dutch_traces = FALSE, # logical: use dutch eligibility traces?, only relevant in on-policy algorithm
							  # policy = "greedy",  # logical: use boltzmann to select action probabilistically
							  convergence_chunk_length = 10000, # length of block that is checked against convergence
							  convergence_cycle_length = 10, # what is the maximum cycle length considered
							  convergence_check_frequency = 2000, # how often should convergence be checked
							  save_single_runs = FALSE,            # logical, TRUE: save simulation results to disk, FALSE: return as list
							  varied_parameter = character(0),     # helper for file management
							  ... # further arguments passed to economic environment
) {
	
	# Garbage collection
	gc()
	
	# workaround to ensure all required functions are loaded on workers
	a <- select_action_on_policy_greedy;a <-  select_action_expected_greedy; a <-  select_action_tree_backup_greedy; a <-  td_error_on_policy_differential; a <-  td_error_on_policy_discounted; a <- td_error_on_policy_discounted; a <- td_error_expected_discounted; a <-  td_error_expected_differential; a <- td_error_tree_backup_discounted; a <- td_error_tree_backup_differential

	# calculate benchmark prices (nash and collusive), extract marginal costs from specification
	p_n <- nash_prices(...)
	p_m <- optimize_joint_profits(...)
	mc <- list(...)$c	
	
	# determine feasibe prices
	max_price <- p_m + zeta * (p_n - mc)
	available_prices <- round(seq(from = mc, to = max_price, length.out = m), rounding_precision)

	# calculate reward set for all action combinations, i.e. cartesian product of available prices
	R <- cross2(available_prices, available_prices) %>%
		map(as.numeric) %>% 
		map(calculate_profits, ...)
	
	# dependent on selected feature extraction method, prepare simulation and determine length of w vector
	if (features_by == "tiling") {
		
		get_x <<- get_x_tiling
		
		feature_specs <- set_up_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 3)
		
		length_w <- specifications$n_tiles^3 * specifications$n_tilings
	} else if (features_by == "tabular") {
		
		get_x <<- get_x_tiling
		
		# as a special case of tiling, if tabular is selected, specifications are overwritten accordingly
		specifications$n_tilings <- 1
		specifications$n_tiles <- m
		
		feature_specs <- set_up_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 3)
		
		length_w <- specifications$n_tiles^3 * specifications$n_tilings
	}  else if (features_by == "poly_separated"){
		
		# 'memoise' ensures the function caches results such that the same input parameters needn't be executed twice.
		# Benchmarking suggested that this is faster for polynomials, but not for tiling/tabular
		get_x <<- memoise(get_x_poly_separate)
		
		feature_specs <- set_up_poly_separate(specifications = specifications, available_prices)
		
		length_w <- (choose(specifications$degree_sep + 2, 2) - 1) * m
	} else if (features_by == "poly_tiling") {
		get_x <<- memoise(get_x_poly_tilings)
		
		feature_specs <- set_up_poly_tilings(specifications = specifications, min_price = mc, max_price = max_price, vars = 3)
		
		length_states <- 3  # two states + 1 action
		length_w <- (choose(specifications$degree_poly_tiling + length_states, length_states) - 1) *
			specifications$poly_n_tilings * specifications$poly_n_tiles^length_states
		
	} else {
		stop("features_by must be one of 'tiling', 'tabular', `poly_separated` or 'poly_tiling'")
	}
	
	# initialize weights/coefficients for both players
	w_single <- rep(w_init, length_w)
	w <- list(w_single, w_single)
	
	# initiate eligibility trace for both players
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
	
	# retrieve utilized type of td-error calculation method
	td_error <- str_c("td_error", Algorithm, td_error_method, sep = "_") %>% get()
	
	# initialize argument passed continuously to td_error function
	if (td_error_method == "differential") {
		r_bar <- rep(0, 2)
		TDs <- list(list(Error = 0, r_bar = 0, Gamma = Gamma), list(Error = 0, r_bar = 0, Gamma = Gamma))
	} else {
		TDs <- list(list(Error = 0), list(Error = 0))
	}
	
	

# Policy ------------------------------------------------------------------

	# if(!(policy %in% c("boltzmann", "greedy"))) {
	# 	stop("policy must be 'greedy' or 'boltzmann'")
	# }

	# retrieve utilized selection action method
	select_action <- get(str_c("select_action", Algorithm, "greedy", sep = "_"))
	
	# pre-determine exploration rate
	if(is.na(Epsilon_constant)) {
		
		# time declining exploration rate controled by psi and beta
		epsilon <- Psi * exp(-Beta * 1:TT)
		
	} else {
		
		# constant exploration rate
		epsilon <- rep(Epsilon_constant, TT)
	}
	
	

# Set up trackers ---------------------------------------------------------

	t <- 1  # tracks time
	outcomes <- matrix(NA, nrow = TT + TT_intervention + 1, ncol = 4,
							 dimnames = list(NULL, map(c("price", "profit"), ~str_c(., 1:2, sep = "_")) %>% unlist()))
	convergence <- list(converged = FALSE)
	

# Sample Initial State ----------------------------------------------------

	if (!is.na(seed)) {set.seed(seed)}
	initial_state_ids <- sample(1:m, 2, replace = TRUE)
	s_t <- available_prices[initial_state_ids]
	

# execute simulation with specified learning algorithm ------------------------------
	
	# reduce environment to list. That list is then passed to the iterative loop
	environment_initialization <- mget(ls())
	
	# run iterative loop, save results as environment_convergence (a list)
	if (Algorithm == "on_policy") {
		environment_convergence <- SARSA(passed_environment = environment_initialization)
		
	} else if (Algorithm %in% c("expected", "tree_backup")) {
		environment_convergence <- expected_SARSA(passed_environment = environment_initialization)
	}
	
	# list2env(x = environment_convergence, envir = environment())
	
	# print("intervention")

# Manual Intervention ------------------------------------------------------

	# pass convergence environment (list) to intervention loop where one agent deviates
	environment_intervention <- intervention(passed_environment = environment_convergence)
	
	# unpack list to environment
	list2env(x = environment_intervention, envir = environment())


# Return Results ----------------------------------------------------------
	
	# truncate from outcomes NA rows 'after convergence'
	outcomes <- na.omit(outcomes)
	
	# if(is.na(run_id)) {run_id <- sample.int(1000000, size = 1)}
	
	
	# summarize run in 'res'
	
	res <- list(outcomes = outcomes, w = w, timestamp = Sys.time(),
					available_prices = available_prices, run_id = run_id,
					specs = as.list(match.call()),
					feature_specs = feature_specs,
					convergence = convergence,
					get_x = get_x)
	
	# if save is requested...
	if (save_single_runs) {
		
		folder <- str_c("simulation_results/", varied_parameter, "/")
		
		# ... create directory, if non-existent, ...
		if(!dir.exists(folder)) {
			print(str_c("creating folder here: ", folder))
			folder_success <- dir.create(folder, recursive = TRUE)
			print(str_c("Success: ", folder_success))
		}
		
		# ...save compressed file in directory
		save(res, file = str_c(folder,
									  features_by, "_",
									  get(varied_parameter), "_",
									  run_id,
									  ".RData"),
			  compress = TRUE)
		
		# ... return nothing
		return(NULL)
		
		
		# else return results
	} else {
		return(res)
	}
	
	# release cache
	if(is.memoised(get_x)) {
		forget(get_x)
	}
}
