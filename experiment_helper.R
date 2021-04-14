# Error Recovery ----------------------------------------------------------


single_run_with_recovery <- function(...) {
	try(single_run(...))
}



# Single Experiment -------------------------------------------------------


single_experiment <- function(experiment, static_specs, runs) {
	
	
	experiment_specs <- map(experiment,
									.f = rep,
									length.out = runs) %>%
		c(list(run_id = 1:runs))
	
	
	print(str_c("Starting Experiment. Features_by = ", experiment$features_by,
					"   | Alpha = ", experiment$Alpha,
					"   | Beta = ", experiment$Beta,
					"   | Delta = ", experiment$Delta,
					"   | Lambda = ", experiment$Lambda))
	
	# retrieve number of cores and specify requested number of runs
	(no_of_cores <- detectCores(all.tests = TRUE, logical = FALSE))
	plan(strategy = cluster, workers = no_of_cores)   # THIS MAY BE ADJUSTED
	
	res <- future_.mapply(
		FUN = single_run_with_recovery,
		dots = experiment_specs,
		MoreArgs = static_specs,
		future.seed = 123456
	)
	
	# # save and return results
	save(res, file = str_c("simulation_results/separate/", str_replace_all(as.character(now()), "[[:blank:]|[:punct]]", "_") , ".RData"))
	return(res)
	
}



# Varying Parameters --------------------------------------------------


vary_alpha <- function(feature_by, variable_specs, no_vary = NULL, static_specs, runs) {
	
	experiment_sequence <- c(features_by = feature_by, variable_specs)
	
	
	l <- map_int(experiment_sequence,
					 .f = length) %>%
		prod()
	
	experiment_sequence_specs <- map(experiment_sequence,
												.f = rep,
												length.out = l) %>% transpose()
	
	map(.x = experiment_sequence_specs,
		 .f = single_experiment,
		 static_specs = static_specs,
		 runs = runs)
}




vary_parameter <-  function(feature_by, alpha, variable_specs, no_vary = NULL, static_specs, runs) {
	
	cleaned_variable_specs <- discard(variable_specs, names(variable_specs) == "Alpha")
	
	experiment_sequence <- c(features_by = feature_by, Alpha = alpha, cleaned_variable_specs)
	
	
	l <- map_int(experiment_sequence,
					 .f = length) %>%
		prod()
	
	experiment_sequence_specs <- map(experiment_sequence,
												.f = rep,
												length.out = l) %>% transpose()
	
	map(.x = experiment_sequence_specs,
		 .f = single_experiment,
		 static_specs = static_specs,
		 runs = runs)
}




# Alpha -------------------------------------------------------------------

experiment_alpha <- function(alpha, features_by, runs, m, TT) {
	
	print(str_c("starting 'Alpha' experiment. Features_by = ", features_by, "    | Alpha = ", alpha))
	
	# retrieve number of cores and specify requested number of runs
	(no_of_cores <- detectCores(all.tests = TRUE, logical = FALSE))
	plan(strategy = cluster, workers = no_of_cores)   #THIS MAY BE ADJUSTED
	
	res <- future_lapply(X = 1:runs,
								FUN = single_run_with_recovery,
								future.seed = 123456,
								Algorithm = "expected",
								n = 2,
								zeta = 1,
								rounding_precision = 8,
								m = m,
								TT = TT,
								TT_intervention = 10,
								Alpha =  alpha,
								Beta = 1*10^-4,
								Gamma = 0.05,
								Delta = 0.95,
								Lambda = 0.5,
								Epsilon_constant = NA,
								Psi = 0.7,
								w_init = 0,
								r_adjust = 0.2229272,
								seed = NA,
								features_by = features_by,
								specifications = list(
									degree_sep = 4,
									degree_poly_tiling = 4,
									poly_n_tilings = 4,
									poly_n_tiles = 3,
									n_tilings = 5,
									n_tiles = 5
								),
								td_error_method = "discounted",
								dutch_traces = TRUE,
								policy = "greedy",
								convergence_chunk_length = 1000,
								convergence_cycle_length = 10,
								convergence_check_frequency = 1000,
								c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)
	
	# save and return results
	save(res, file = str_c("simulation_results/separate/alpha_", features_by, "_", str_replace(as.character(alpha), "\\.", "_"), ".RData"))
	return(res)
}




vary_alpha <- function(experiment_specs, runs, m, TT) {
	map(.x = experiment_specs$alphas,
		 .f = experiment_alpha,
		 features_by = experiment_specs$features,
		 runs = runs,
		 m = m,
		 TT = TT)
}



# temp <- vary_alpha(
# 	experiment_specs = list(
# 		features = "tabular",
# 		alphas = alphas
# 		),
# 	runs = 4,
# 	m = 11)





# Lambda ------------------------------------------------------------------


experiment_lambda <- function(lambda, alpha, features_by, runs, m, TT) {
	
	print(str_c("starting 'Lambda' experiment. Features_by = ", features_by, "    | Lambda = ", lambda, "  |  Alpha = ", alpha))
	
	# retrieve number of cores and specify requested number of runs
	(no_of_cores <- detectCores(all.tests = TRUE, logical = FALSE))
	plan(strategy = cluster, workers = no_of_cores)   #THIS MAY BE ADJUSTED
	
	
	res <- future_lapply(X = 1:runs,
								FUN = single_run_with_recovery,
								future.seed = 123456,
								Algorithm = "expected",
								n = 2,
								zeta = 1,
								rounding_precision = 8,
								m = m,
								TT = TT,
								TT_intervention = 10,
								Alpha =  alpha,
								Beta = 1*10^-4,
								Gamma = 0.05,
								Delta = 0.95,
								Lambda = lambda,
								Epsilon_constant = NA,
								Psi = 0.7,
								w_init = 0,
								r_adjust = 0.2229272,
								seed = NA,
								features_by = features_by,
								specifications = list(
									degree_sep = 4,
									degree_poly_tiling = 4,
									poly_n_tilings = 4,
									poly_n_tiles = 3,
									n_tilings = 5,
									n_tiles = 5
								),
								td_error_method = "discounted",
								dutch_traces = TRUE,
								policy = "greedy",
								convergence_chunk_length = 1000,
								convergence_cycle_length = 10,
								convergence_check_frequency = 1000,
								c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)
	
	# save and return results
	save(res, file = str_c("simulation_results/separate/lambda_", features_by, "_", str_replace(as.character(lambda), "\\.", "_"), ".RData"))
	return(res)
}


vary_lambda <- function(experiment_specs, runs, m, TT) {
	map(.x = experiment_specs$lambda,
		 .f = experiment_lambda,
		 alpha = experiment_specs$alpha,
		 features_by = experiment_specs$features,
		 runs = runs,
		 m = m,
		 TT = TT)
}




# GENERAL




