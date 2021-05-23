print("first line, specifying no_of_cores and total runs per experiment")

# High Level simulation specifications ------------------------------------

runs_per_experiment <- 48 # repetitions per experiment (same set of specifications)
no_of_cores <- 16


print("loading libraries")

library(rlist)
library(tidyverse)
library(parallel)
library(future.apply)
library(memoise)
library(nnet)


print(str_c("Number of detected phsyical cores: ", detectCores(all.tests = TRUE, logical = FALSE)))
print(str_c("running simulation on ", no_of_cores, " core(s)"))
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
features_extraction_methods <- c("tabular", "tiling", "poly_separated", "poly_tiling")


# static specs (no variation in study whatsoever)
static_specs <- list(
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
	convergence_chunk_length = 10000,
	convergence_cycle_length = 10,
	convergence_check_frequency = 2000,
	save_single_runs = TRUE,
	c = 1, a = 2, a_0 = 0, mu = 0.25
)


# variable parameters (some sort of variation throughout study, the baseline will be adjusted throughout simulations)

baseline <- list(
	Algorithm = "expected",
	Alpha = NA,
	Beta = 4*10^-5,
	Gamma = 0.95,
	Lambda = 0.5,
	Upsilon = 0.05,
	td_error_method = "discounted",
	Psi = 1,
	zeta = 1,
	m = 19,
	TT = 500000,
	length_prolonged_intervention = FALSE
)







# Alpha -------------------------------------------------------------------


# alphas <- 1 * 10^-c(1,2,3,4,5,6,7,8,10,12)
# alpha_input <- list_modify(baseline, Alpha = alphas)
# 
# walk(.x = features_extraction_methods,
# 	  .f = vary_alpha,
# 	  variable_specs = alpha_input,
# 	  static_specs = static_specs,
# 	  runs = runs_per_experiment,
# 	  no_of_cores = no_of_cores)


alphas_manually_optimized <- c(0.1, 0.001, 1 * 10^-6, 1 * 10^-8)


# prolonged deviation with optimized Alphas ----------------------------

# prolonged_intervention_input <- list_modify(baseline, Alpha = NULL, length_prolonged_intervention = list(1:10))
# 
# 
# walk2(.x = features_extraction_methods,
# 		.y = alphas_manually_optimized,
# 		.f = prolonged_intervention,
# 		variable_specs = prolonged_intervention_input,
# 		static_specs = static_specs,
# 		runs = runs_per_experiment,
# 		no_of_cores = no_of_cores)



# Lambda ------------------------------------------------------------------

# lambdas <- c(seq(from = 0, to = 0.8, by = 0.2), 0.9)
# lambda_input <- list_modify(baseline, Alpha = NULL, Lambda = lambdas)
# 
# 
# walk2(.x = features_extraction_methods,
# 		.y = alphas_manually_optimized,
# 		.f = vary_parameter,
# 		variable_specs = lambda_input,
# 		static_specs = static_specs,
# 		runs = runs_per_experiment,
# 		no_of_cores = no_of_cores)


# # Gamma -------------------------------------------------------------------

# gammas <- c(0L, 0.25, 0.5, 0.75, 0.9, 1L)
# gamma_input <- list_modify(baseline, Alpha = NULL, Gamma = gammas)
# 
# walk2(.x = features_extraction_methods,
# 		.y = alphas_manually_optimized,
# 		.f = vary_parameter,
# 		variable_specs = gamma_input,
# 		static_specs = static_specs,
# 		runs = runs_per_experiment,
# 		no_of_cores = no_of_cores)



# Beta ------------------------------------------------------------------

# betas <- c(16 * 10^-5, 8 * 10^-5, 2 * 10^-5, 1 * 10^-5)
# TTs <- c(1.25 * 10^5, 2.5 * 10^5, 10^6, 2 * 10^6)
# (exp(-betas * TTs)) # equal probability of exploitaiton at the last possible learning period
# 
# beta_input <- list_modify(baseline, Alpha = NULL, Beta = betas, TT = TTs)
# 
# walk2(.x = features_extraction_methods,
# 		.y = alphas_manually_optimized,
# 		.f = vary_parameter,
# 		variable_specs = beta_input,
# 		static_specs = static_specs,
# 		runs = runs_per_experiment,
# 		no_of_cores = no_of_cores)


# # Vary m (number of feasible prices) -----------------------------------------------
# 
# number_of_prices <- c(10, 39, 63)
# 
# m_input <- list_modify(baseline, Alpha = NULL,
# 									 m = number_of_prices)
# 
# 
# walk2(.x = features_extraction_methods,
# 		.y = alphas_manually_optimized,
# 		.f = vary_parameter,
# 		variable_specs = m_input,
# 		static_specs = static_specs,
# 		runs = runs_per_experiment,
# 		no_of_cores = no_of_cores)


# 
# 
# 
# # Zeta --------------------------------------------------------------------


zetas <- c(0.1, 0.5, 1, 1.5)
zeta_input <- list_modify(baseline, Alpha = NULL,
								 zeta = zetas)


walk2(.x = features_extraction_methods,
		.y = alphas_manually_optimized,
		.f = vary_parameter,
		variable_specs = zeta_input,
		static_specs = static_specs,
		runs = runs_per_experiment,
		no_of_cores = no_of_cores)


# Vary Algorithm ----------------------------------------------------------

# tree backup
# tb_input <- list_modify(baseline, Alpha = NULL, Algorithm = "tree_backup")
# 
# walk2(.x = features_extraction_methods,
# 		.y = alphas_manually_optimized,
# 		.f = vary_algorithm,
# 		variable_specs = tb_input,
# 		static_specs = static_specs,
# 		runs = runs_per_experiment,
# 		no_of_cores = no_of_cores)


# on policy (common SARSA)
# op_input <- list_modify(baseline, Alpha = NULL, Algorithm = "on_policy")
# 
# walk2(.x = features_extraction_methods,
# 		.y = alphas_manually_optimized,
# 		.f = vary_algorithm,
# 		variable_specs = op_input,
# 		static_specs = static_specs,
# 		runs = runs_per_experiment,
# 		no_of_cores = no_of_cores)




# 
# # Differential Reward Setting --------------------------------------------------
# 
# 
# upsilons <- c(0.001, 0.005, 0.01, 0.025, 0.05, 0.1)
# upsilon_input <- list_modify(baseline, Alpha = NULL,
# 									  td_error_method = "differential",
# 									  Upsilon = upsilons)
# 
# 
# 
# 
# walk2(.x = features_extraction_methods,
# 		.y = alphas_manually_optimized,
# 		.f = vary_parameter,
# 		variable_specs = upsilon_input,
# 		static_specs = static_specs,
# 		runs = runs_per_experiment,
# 		no_of_cores = no_of_cores)





print(str_c(Sys.time(), " | script over"))
		