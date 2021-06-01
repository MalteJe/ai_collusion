library(tidyverse)
library(janitor)
library(future.apply)
library(glue)
library(parallel)

get_convergence_t <- function(run) {
	if (run$convergence$converged) {
		return(run$convergence$convergence_t)
	} else {
		return(nrow(run$outcomes) - run$specs$TT_intervention)
	}
}



get_delta <- function(profit) {
	(profit - 0.2229272) / (0.3374905 - 0.2229272)
}


load_return <- function(path, t_grouping, t_before_intervention = 9, t_profit = 100) {
	
	if(is.na(path)) {
		return(list(
			outcomes = NA,
			avg_profits = NA,
			avg_prices = NA,
			intervention = NA,
			intervention_prolonged = NA,
			convergence = NA,
			cycle_length = NA
		))
	} else {
		load(path)
		
		conv_t <- get_convergence_t(res)
		
		intervention <- res$outcomes[(conv_t-t_before_intervention):nrow(res$outcomes),] %>%
			as_tibble() %>%
			mutate(tau = row_number() - t_before_intervention - 1)
		
		outcomes <- as_tibble(res$outcomes) %>%
			mutate(t = row_number(),
			t_group = (t-1) %/% t_grouping + 1,
			price = (price_1 + price_2)/2,
			profit = (profit_1 + profit_2)/2
			) %>%
			filter(t <= conv_t) %>%
			group_by(t_group) %>%
			summarize_at(c("price", "profit"), mean) %>%
			mutate(Delta = get_delta(profit)) %>%
			select(-profit)
		
		avg_profits <- tail(res$outcomes, t_profit + res$specs$TT_intervention)[, c("profit_1", "profit_2")] %>%
			head(t_profit) %>%
			mean()
		
		avg_prices <- tail(res$outcomes, t_profit + res$specs$TT_intervention)[, c("price_1", "price_2")] %>%
			head(t_profit) %>%
			mean()
		
		return(list(
			outcomes = outcomes,
			avg_profits = avg_profits,
			avg_prices = avg_prices,
			intervention = intervention,
			intervention_prolonged = res$intervention_prolonged,
			convergence = conv_t,
			cycle_length = res$convergence$cycle_length))
		
		rm(res)
		rm(outcomes)
		gc()
	}
}

# load("simulation_results/prolonged_deviation/tiling_optimized_6.RData")
# load_return("simulation_results/prolonged_deviation/tiling_optimized_6.RData", t_grouping = 50000)
# load_return("simulation_results/Beta_final/tabular_1e-05_1.RData", t_grouping = 50000)


# specify path, list single runs in directory and adjust names

# load aggregate_save loads single runs in parallel, aggregates the data as necessary, saves them to the directory and returns a summary
load_aggregate_save <- function(experiment_job, t_grouping) {
	
	# concatenate path
	path <- str_c("simulation_results/", experiment_job, "/")
	
	# extract filenames
	filenames <- list.files(path) %>%
		str_subset("RData$") %>%
		str_subset("aggregated", negate = TRUE)
	
	print(glue("detected {length(filenames)} files."))
	
	# wrangle filenames into 'nicer' names
	adjusted_files <- str_replace(filenames, "poly_tiling", "poly-tiling")  %>%
		str_replace("poly_separate", "poly-separate") %>%
		str_sub(end = -7L)
	
	# extract meta_overview
	meta_overview <- tibble(filename = adjusted_files) %>%
		separate(col = filename, into = c("feature_method", "varied_parameter", "run_id"), sep = "_") %>%
		mutate(path = str_c(path, filenames), successful = TRUE,
				 FEM = case_when(                             # ensure consistent naming with paper
				 	feature_method == "tiling" ~ "Tile Coding",
				 	feature_method == "tabular" ~ "Tabular",
				 	feature_method == "poly-tiling" ~ "Polynomial Tiles",
				 	feature_method == "poly-separated" ~ "Separate Polynomials",
				 	TRUE ~ NA_character_
				 )) 
	
	# count runs per experiment (this will be returned)
	out <- meta_overview %>%
		group_by(FEM, varied_parameter) %>%
		count() %>%
		arrange(n)
	
	# extract unique values of varied parameter
	variations <- sort(unique(as.numeric(meta_overview$varied_parameter)), decreasing = TRUE)
	print(glue("The following variations have been detected: {str_flatten(variations, collapse = ', ')}"))
	
	# load simulations in parallel
	plan(strategy = cluster, workers = detectCores(all.tests = TRUE, logical = FALSE))
	print("Loading and aggregating runs in parallel. This will take a couple of minutes!")
	data_nested <- meta_overview %>%
		mutate(sim = future_lapply(X = path, FUN = load_return, t_grouping = t_grouping)) %>%
		select(-path)
	
	# unnest data
	print("unnesting data")
	data <- data_nested %>%
		unnest_wider(sim) %>%
		mutate(FEM = fct_relevel(FEM, "Tabular", "Tile Coding", "Separate Polynomials", "Polynomial Tiles"),
				 varied_parameter_fct = fct_relevel(as_factor(varied_parameter), as.character(variations)))
	
	print("saving data in directory")
	# save aggregated data in same directory, return summary
	save(data, file = str_c(path, "aggregated.RData"))
	
	return(out)
}

# load simulation results
load_aggregate_save("Alpha_final", t_grouping = 50000)
load_aggregate_save("prolonged_deviation", t_grouping = 50000)   #warning message due to no variation --> can be ignored
load_aggregate_save("Beta_final", t_grouping = 50000)
load_aggregate_save("Lambda_final", t_grouping = 50000)
load_aggregate_save("m_final", t_grouping = 50000)
load_aggregate_save("zeta_final", t_grouping = 50000)
load_aggregate_save("Gamma_final", t_grouping = 50000)
load_aggregate_save("tree_backup_final", t_grouping = 50000)
load_aggregate_save("on_policy_final", t_grouping = 50000)
load_aggregate_save("upsilon_final", t_grouping = 50000)
