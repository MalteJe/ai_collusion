print("first line, loading libraries")

library(rlist)
library(tidyverse)
library(parallel)
library(future.apply)
library(nnet)
library(memoise)

print(str_c("Number of detected phsyical cores: ", detectCores(all.tests = TRUE, logical = FALSE)))

print(str_c("loading other scripts from ", getwd()))

getwd() %>%
	list.files(full.names = TRUE) %>%
	str_subset("^(?!.*master.R$)") %>% 
	str_subset("^(?!.*static_visualizations.R$)") %>% 
	str_subset("^(?!.*aggregate_files.R$)") %>% 
	str_subset("^(?!.*playground.R$)") %>% 
	str_subset(".R$") %>%
	walk(source)


# For Finalization --------------------------------------------------------

print("defining specs")

# methods
features_extraction_methods <- c("poly_separated")

# static specs (no variation in study whatsoever)
static_specs <- list(
	Algorithm = "expected",
	n = 2,
	rounding_precision = 8,
	TT_intervention = 10,
	Epsilon_constant = NA,
	w_init = 0,
	r_adjust = 0.2229272,
	seed = NA,
	specifications = list(
		# degree = 6,
		degree_sep = 5,
		degree_poly_tiling = 4,
		poly_n_tilings = 5,
		poly_n_tiles = 4,
		n_tilings = 5,
		n_tiles = 8
	),
	dutch_traces = TRUE,
	policy = "greedy",
	convergence_chunk_length = 10000,
	convergence_cycle_length = 10,
	convergence_check_frequency = 2000,
	save_single_runs = TRUE,
	c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25
)


# variable_specs (some sort of variation throughout study, the baseline will be adjusted throughout simulations)

baseline <- list(
	Alpha = NA,
	Beta = 4*10^-5,
	Gamma = 0.05,
	Delta = 0.95,
	Lambda = 0.5,
	td_error_method = "discounted",
	Psi = 1,
	zeta = 1,
	m = 19,
	TT = 500000
)


# repetitions per experiment (same set of specifications)
runs_per_experiment <- 48




# Alpha -------------------------------------------------------------------


alphas <- 1 * 10^-c(6, 8, 10, 12)
alpha_input <- list_modify(baseline, Alpha = alphas)

print("defined specs, starting simulations")

walk(.x = features_extraction_methods,
	  .f = vary_alpha,
	  variable_specs = alpha_input,
	  static_specs = static_specs,
	  runs = runs_per_experiment,
	  no_of_cores = 16)

# names(meta_res_alpha) <- features_extraction_methods
# 	
# save(meta_res_alpha, file = "simulation_results/res_varied_alpha.RData")




# 
# # Lambda ------------------------------------------------------------------
# 
# lambdas <- seq(from = 0, to = 0.8, by = 0.2)
# lambda_input <- list_modify(baseline, Alpha = NULL, Lambda = lambdas)
# alphas_manually_optimized <- c(0.25, 1 * 10^-4, 1 * 10^-6, 1 * 10^-6)
# 
# 
# meta_res_lambda <- map2(.x = features_extraction_methods,
# 								.y = alphas_manually_optimized,
# 								.f = vary_parameter,
# 								variable_specs = lambda_input,
# 								static_specs = static_specs,
# 								runs = runs_per_experiment,
# 								sequential_execution = TRUE)
# 
# 
# 
# names(meta_res_lambda) <- features_extraction_methods
# 
# 
# save(meta_res_lambda, file = "simulation_results/res_varied_lambda.RData")
# 
# 
# 
# # Delta -------------------------------------------------------------------
# 
# deltas <- c(0L, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
# delta_input <- list_modify(baseline, Alpha = NULL,
# 									Delta = deltas)
# 
# 
# meta_res_delta <- map2(.x = features_extraction_methods,
# 								.y = alphas_manually_optimized,
# 								.f = vary_parameter,
# 								variable_specs = delta_input,
# 								static_specs = static_specs,
# 								runs = runs_per_experiment,
# 								sequential_execution = TRUE)
# 
# 
# 
# names(meta_res_delta) <- features_extraction_methods
# 
# 
# save(meta_res_delta, file = "simulation_results/res_varied_delta.RData")
# 
# 
# # Psi -------------------------------------------------------------------
# 
# psis <- c(1, 0.8, 0.6, 0.4, 0.2)
# psi_input <- list_modify(baseline, Alpha = NULL,
# 									Psi = psis)
# 
# 
# meta_res_psi <- map2(.x = features_extraction_methods,
# 							  .y = alphas_manually_optimized,
# 							  .f = vary_parameter,
# 							  variable_specs = psi_input,
# 							  static_specs = static_specs,
# 							  runs = runs_per_experiment,
# 							sequential_execution = TRUE)
# 
# 
# 
# names(meta_res_psi) <- features_extraction_methods
# 
# 
# save(meta_res_psi, file = "simulation_results/res_varied_psi.RData")
# 
# 
# 
# # Zeta --------------------------------------------------------------------
# 
# zetas <- c(0.1, 0.5, 1, 2)
# zeta_input <- list_modify(baseline, Alpha = NULL,
# 								 zeta = zetas)
# 
# 
# meta_res_zeta <- map2(.x = features_extraction_methods,
# 							.y = alphas_manually_optimized,
# 							.f = vary_parameter,
# 							variable_specs = zeta_input,
# 							static_specs = static_specs,
# 							runs = runs_per_experiment,
# 							sequential_execution = TRUE)
# 
# 
# 
# names(meta_res_zeta) <- features_extraction_methods
# 
# save(meta_res_zeta, file = "simulation_results/res_varied_psi.RData")
# 
# 
# 
# # Differential Reward Setting --------------------------------------------------
# 
# 
# gammas <- c(0.001, 0.005, 0.01, 0.03, 0.05, 0.1)
# gamma_input <- list_modify(baseline, Alpha = NULL,
# 									td_error_method = "differential",
# 								  gamma = gammas)
# 
# 
# meta_res_gamma <- map2(.x = features_extraction_methods,
# 							 .y = alphas_manually_optimized,
# 							 .f = vary_parameter,
# 							 variable_specs = gamma_input,
# 							 static_specs = static_specs,
# 							 runs = runs_per_experiment,
# 							 sequential_execution = TRUE)
# 
# 
# 
# names(meta_res_gamma) <- features_extraction_methods
# 
# save(meta_res_gamma, file = "simulation_results/res_varied_gamma.RData")
# 
# 
# 
# 
# # Vary m (number of feasible prices) -----------------------------------------------
# 
# number_of_prices <- c(4, 10, 25, 39, 63)
# number_of_prices <- c(4, 10, 25, 39)
# 
# m_input <- list_modify(baseline, Alpha = NULL,
# 									 m = number_of_prices)
# 
# 
# meta_res_m <- map2(.x = features_extraction_methods,
# 						 .y = alphas_manually_optimized,
# 						 .f = vary_parameter,
# 						 variable_specs = m_input,
# 						 static_specs = static_specs,
# 						 runs = runs_per_experiment,
# 						 sequential_execution = TRUE)
# 
# names(meta_res_m) <- features_extraction_methods
# 
# save(meta_res_m, file = "simulation_results/res_varied_m.RData")


print(str_c(Sys.time(), " | script over"))
		