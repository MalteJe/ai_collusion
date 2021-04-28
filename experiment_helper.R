# Error Recovery ----------------------------------------------------------


single_run_with_recovery <- function(...) {
	try(single_run(...))
}

# Single Experiment -------------------------------------------------------




single_experiment <- function(experiment, static_specs, runs, no_of_cores, varied_parameter) {
	
	# garbage collection
	gc()
	
	experiment_specs <- map(experiment,
									.f = rep,
									length.out = runs) %>%
		c(list(run_id = 1:runs))
	
	
	print(str_c(Sys.time(),
					" - Starting Experiment. Features_by = ", experiment$features_by,
					"  | Varied Parameter: ", varied_parameter, 
					"  | Value: ", experiment[varied_parameter],
					"   | Runs: ", runs))
	
	# retrieve number of cores and specify requested number of runs
	
	if (no_of_cores == 1) {
		print("executing sequential")
		plan(strategy = sequential)
	} else {
		print("executing parallel")
		plan(strategy = cluster, workers = no_of_cores) 
	}
	
	future_.mapply(
		FUN = single_run_with_recovery,
		dots = experiment_specs,
		MoreArgs = c(static_specs, varied_parameter = varied_parameter),
		future.seed = 123456
	)
	
	# # save and return results
	# save(res, file = str_c("simulation_results/separate/", str_replace_all(as.character(now()), "[[:blank:]|[:punct]]", "_") , ".RData"))
	# return(res)
	
}



# Varying Parameters --------------------------------------------------


vary_alpha <- function(feature_by, variable_specs, static_specs, runs, no_of_cores = 1) {
	
	experiment_sequence <- c(features_by = feature_by, variable_specs)
	
	l <- map_int(experiment_sequence,
					 .f = length) %>%
		prod()
	
	experiment_sequence_specs <- map(experiment_sequence,
												.f = rep,
												length.out = l) %>% purrr::transpose()
	
	
	
	map(.x = experiment_sequence_specs,
		 .f = single_experiment,
		 static_specs = static_specs,
		 runs = runs,
		 no_of_cores = no_of_cores,
		 varied_parameter = names(keep(variable_specs, .p = ~length(.) > 1)))
}




vary_parameter <-  function(feature_by, alpha, variable_specs, no_vary = NULL, static_specs, runs, no_of_cores = 1) {
	
	cleaned_variable_specs <- discard(variable_specs, names(variable_specs) == "Alpha")
	
	experiment_sequence <- c(features_by = feature_by, Alpha = alpha, cleaned_variable_specs)
	
	
	l <- map_int(experiment_sequence,
					 .f = length) %>%
		prod()
	
	experiment_sequence_specs <- map(experiment_sequence,
												.f = rep,
												length.out = l) %>% purrr::transpose()
	
	map(.x = experiment_sequence_specs,
		 .f = single_experiment,
		 static_specs = static_specs,
		 runs = runs,
		 no_of_cores = no_of_cores,
		 varied_parameter = names(keep(variable_specs, .p = ~length(.) > 1)))
}
