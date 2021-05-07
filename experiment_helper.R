# Error Recovery ----------------------------------------------------------

# run simulation but recover from errors
single_run_with_recovery <- function(...) {
	try(single_run(...))
}


# Single Experiment -------------------------------------------------------

# single_experiment executes specified number of runs for a particular experiment, i.e. a specific combination of parameters and feature extraction method
single_experiment <- function(experiment, static_specs, runs, no_of_cores, varied_parameter) {
	
	# garbage collection
	gc()
	
	# replicate experiment specifications and append run id 
	experiment_specs <- map(experiment,
									.f = rep,
									length.out = runs) %>%
		c(list(run_id = 1:runs))
	
	# determine strategy (cluster vs. sequential execution) dependent on number of specified cores
	
	if (no_of_cores == 1) {
		plan(strategy = sequential)
	} else {
		plan(strategy = cluster, workers = no_of_cores)
	}
	
	# print informative message to console
	print(str_c(Sys.time(),
					" - Starting Experiment. Features_by = ", experiment$features_by,
					"  | Varied Parameter: ", varied_parameter, 
					"  | Value: ", experiment$varied_parameter,
					"   | Runs: ", runs))
	
	# map every runs specification to single_run_with recovery.
	# 'future.apply' package allows for consistent results between sequential and parallel execution.
	future_.mapply(
		FUN = single_run_with_recovery,
		dots = experiment_specs,
		MoreArgs = c(static_specs, varied_parameter = varied_parameter),
		future.seed = 123456
	)
	
}



# Varying Parameters --------------------------------------------------

# vary_alpha prepares the specifications for various alpha
vary_alpha <- function(feature_by, variable_specs, static_specs, runs, no_of_cores = 1) {
	
	# concatenate feature extraction method and variable specs and determine number of experiments (i.e. various alphas)
	experiment_sequence <- c(features_by = feature_by, variable_specs)
	
	l <- map_int(experiment_sequence,
					 .f = length) %>%
		prod()
	
	# wrangle specifications in usable format (one element per experiment)
	experiment_sequence_specs <- map(experiment_sequence,
												.f = rep,
												length.out = l) %>% purrr::transpose()
	
	
	# map every requested experiment to single_experiment where all experiment's runs are executed. Return results (if any)
	map(.x = experiment_sequence_specs,
		 .f = single_experiment,
		 static_specs = static_specs,
		 runs = runs,
		 no_of_cores = no_of_cores,
		 varied_parameter = names(keep(variable_specs, .p = ~length(.) > 1)))
}




vary_parameter <-  function(feature_by, alpha, variable_specs, static_specs, runs, no_of_cores = 1) {
	
	# remove alpha specification if exists
	cleaned_variable_specs <- discard(variable_specs, names(variable_specs) == "Alpha")
	
	# concatenate full specifications with optimized alpha
	experiment_sequence <- c(features_by = feature_by, Alpha = alpha, cleaned_variable_specs)
	
	# determine number of experiments (i.e. various alphas)
	l <- map_int(experiment_sequence,
					 .f = length) %>%
		prod()
	
	# wrangle specifications in usable format (one element per experiment)
	experiment_sequence_specs <- map(experiment_sequence,
												.f = rep,
												length.out = l) %>% purrr::transpose()
	
	# map every requested experiment to single_experiment where all experiment's runs are executed. Return results (if any)
	map(.x = experiment_sequence_specs,
		 .f = single_experiment,
		 static_specs = static_specs,
		 runs = runs,
		 no_of_cores = no_of_cores,
		 varied_parameter = names(keep(variable_specs, .p = ~length(.) > 1)))
}

prolonged_intervention <- function(feature_by, alpha, variable_specs, static_specs, runs, no_of_cores =1) {
	
	# remove other alpha specification if exists
	cleaned_variable_specs <- discard(variable_specs, names(variable_specs) == "Alpha")
	
	# concatenate full specifications with optimized alpha
	experiment_sequence <- c(features_by = feature_by, Alpha = alpha, cleaned_variable_specs)
	
	single_experiment(experiment_sequence,
							static_specs,
							runs = runs,
							no_of_cores = no_of_cores,
							varied_parameter = "Alpha_optimized")
}
