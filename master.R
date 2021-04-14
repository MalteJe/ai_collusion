library(rlist)
library(purrr)
library(tidyquant)
library(tidyverse)
library(parallel)
library(future.apply)
library(nnet)
library(viridis)

getwd() %>%
	list.files(full.names = TRUE) %>%
	str_subset("^(?!.*master.R$)") %>% 
	str_subset("^(?!.*static_visualizations.R$)") %>% 
	str_subset(".R$") %>%
	walk(source)






# Gradient Descent --------------------------------------------------------

single_res <- single_run(Algorithm = "expected",
								 n = 2,
								 seed = 1234567,
								 zeta = 1,
								 rounding_precision = 8,
								 m = 11,
								 TT = 1000,
								 TT_intervention = 10,
								 Alpha = 0.02,
								 Beta = 1*10^-4,
								 Gamma = 0.05,
								 Delta = 0.95,
								 Lambda = 0.5,
								 Epsilon_constant = NA,
								 Psi = 0.4,
								 w_init = 0,
								 r_adjust = 0.2229272,
								 run_id = NA,
								 specifications = list(
								 	degree = 4,
								 	splines_degree = 3,
								 	n_knots = 4,
								 	degree_sep = 4,
								 	degree_poly_tiling = 4,
								 	poly_n_tilings = 5,
								 	poly_n_tiles = 4,
								 	n_tilings = 1,
								 	n_tiles = 8
								 ),
								 features_by = "tiling",								 
								 td_error_method = "discounted",
								 dutch_traces = TRUE,
								 policy = "greedy",
								 convergence_chunk_length = 100,
								 convergence_cycle_length = 10,
								 convergence_check_frequency = 200,
								 c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)


# single_simulation_outcomes <- single_res$outcomes

0.7 * exp(- 9 * 10^-6 * c(1, 1000, 10000, 50000, 100000, 150000, 200000, 300000, 1000000))

# retrieve number of cores and specify required number of runs
(no_of_cores <- detectCores(all.tests = TRUE, logical = FALSE))
run_ids <- 1:no_of_cores

# run simulations on cluster with specified number of cores
plan(strategy = cluster, workers = no_of_cores)
meta_res <- future_lapply(X = 1 * 10^-(4:7),
								  FUN = single_run,
								  future.seed = 123456,
								  Algorithm = "expected",
								  n = 2,
								  zeta = 1,
								  rounding_precision = 8,
								  m = 22,
								  TT = 1000000,
								  TT_intervention = 7,
								  # Alpha =  4*10^-5,
								  Beta = 9*10^-6,
								  Gamma = 0.05,
								  Delta = 0.95,
								  Lambda = 0.5,
								  Epsilon_constant = NA,
								  Psi = 0.7,
								  w_init = 0,
								  r_adjust = 0.2229272,
								  seed = NA,
								  run_id = NA,
								  specifications = list(
								  	degree = 4,
								  	splines_degree = 3,
								  	n_knots = 4,
								  	degree_sep = 4,
								  	degree_poly_tiling = 4,
								  	poly_n_tilings = 5,
								  	poly_n_tiles = 5,
								  	n_tilings = 1,
								  	n_tiles = 8
								  ),
								  features_by = "poly_separated",
								  td_error_method = "discounted",
								  dutch_traces = TRUE,
								  policy = "greedy",
								  convergence_chunk_length = 5000,
								  convergence_cycle_length = 10,
								  convergence_check_frequency = 5000,
								  c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)


str(meta_res[[1]])

map(meta_res, .f = ~.$timestamp)
map(meta_res, .f = ~.$w[[1]])
map(meta_res, .f = ~ str(.$specs))


map(meta_res, ~tail(.$outcomes, 20))

source("shiny/visualize_outcomes.R")
shinyApp(ui, server)

map(meta_res, ~.$convergence$convergence_t)

specifications <- list(
	list(degree_sep = 2, degree_poly_tiling = 4, poly_n_tilings = 6, poly_n_tiles = 4, n_tilings = 1, n_tiles = 15, splines_degree = 3, n_knots = 1),
	list(degree_sep = 3, degree_poly_tiling = 4, poly_n_tilings = 4, poly_n_tiles = 5, n_tilings = 2, n_tiles = 10, splines_degree = 3, n_knots = 2),
	list(degree_sep = 4, degree_poly_tiling = 3, poly_n_tilings = 6, poly_n_tiles = 5, n_tilings = 3, n_tiles = 10, splines_degree = 3, n_knots = 3),
	list(degree_sep = 5, degree_poly_tiling = 3, poly_n_tilings = 4, poly_n_tiles = 6, n_tilings = 4, n_tiles = 10, splines_degree = 3, n_knots = 4)
)


save(meta_res, file = "simulation_results/super_long.RData")
meta_res_old <- meta_res




# Policy Optimization -----------------------------------------------------



nd_res <- single_run_nd(n = 2,
								zeta = 1,
								rounding_precision = 6,
								mu_adjust = 1.472982,
								sigma_adjust = -1,
								sigma_control = - 1* 10^-5,
								TT = 5000,
								TT_intervention = 2,
								Alpha = 1 * 10^-5,
								Gamma = NA,
								Delta = 0.95,
								Lambda = 0.5,
								r_adjust = 0.2229272,
								seed = 123,
								run_id = 123,
								specifications = list(
									degree = 4,
									splines_degree = 3,
									n_knots = 4,
									degree_poly_tiling = 3,
									poly_n_tilings = 2,
									poly_n_tiles = 4,
									n_tilings = 4,
									n_tiles = 8
								),
								features_by = "poly_tiling",
								td_error_method = "discounted",
								convergence_chunk_length = 20000000000,
								convergence_cycle_length = 10,
								convergence_check_frequency = 200000000000,
								c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)


# single_simulation_outcomes <- single_res$outcomes

exp(-2) * exp(- 7 * 10^-7 * c(1, 100000, 300000, 1000000))


# retrieve number of cores and specify required number of runs
(no_of_cores <- detectCores(all.tests = TRUE, logical = FALSE))
run_ids <- 1:no_of_cores

# run simulations on cluster with specified number of cores
plan(strategy = cluster, workers = no_of_cores)
meta_res <- future_lapply(X = c("poly", "poly_tiling", "tiling", "splines"),
								  FUN = single_run_nd,
								  future.seed = 123,
								  n = 2,
								  zeta = 1,
								  rounding_precision = 6,
								  mu_adjust = 1.272982,
								  sigma_adjust = -2,
								  sigma_control = 0.05,
								  TT = 1000000,
								  TT_intervention = 10,
								  Alpha = 5 * 10^-7,
								  Gamma = NA,
								  Delta = 0.95,
								  Lambda = 0.5,
								  r_adjust = 0.2229272,
								  seed = NA,
								  run_id = NA,
								  specifications = list(
								  	degree = 6,
								  	splines_degree = 3,
								  	n_knots = 5,
								  	degree_poly_tiling = 4,
								  	poly_n_tilings = 5,
								  	poly_n_tiles = 4,
								  	n_tilings = 4,
								  	n_tiles = 8
								  ),
								  # features_by = "poly_tiling",
								  td_error_method = "discounted",
								  convergence_chunk_length = 20000000000,
								  convergence_cycle_length = 10,
								  convergence_check_frequency = 200000000000,
								  c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)



str(meta_res[[1]])

map(meta_res, .f = ~.$timestamp)
map(meta_res, .f = ~.$w[[1]])
map(meta_res, .f = ~ str(.$specs))


map(meta_res, ~tail(.$outcomes, 20))

source("shiny/visualize_outcomes.R")
shinyApp(ui, server)




# For Finalization --------------------------------------------------------

0.7 * exp(- 1 * 10^-4 * c(1, 1000, 10000, 50000, 100000, 150000, 200000, 300000, 1000000))


# methods
features_extraction_methods <- c("tabular", "tiling", "poly_tiling", "poly_separated")

# static specs (no variation in study whatsoever)
static_specs <- list(
	Algorithm = "expected",
	n = 2,
	zeta = 1,
	rounding_precision = 8,
	TT_intervention = 10,
	Epsilon_constant = NA,
	w_init = 0,
	r_adjust = 0.2229272,
	seed = NA,
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
	c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25
)


# some sort of variation throughout study

baseline <- list(
	Alpha = 1*10^-5,
	Beta = 1*10^-4,
	Gamma = 0.05,
	Delta = 0.95,
	Lambda = 0.5,
	Psi = 0.7,
	m = 11,
	TT = 1000
)






# Alpha -------------------------------------------------------------------


alphas <- c(0.2 * 10^-(0:9))
alpha_input <- list_modify(baseline, Alpha = alphas)


meta_res_alpha <- map(.x = features_extraction_methods,
								  .f = vary_alpha,
								  variable_specs = alpha_input,
								  static_specs = static_specs,
								  runs = 4)

names(meta_res_alpha) <- features_extraction_methods
	
save(meta_res_alpha, file = "simulation_results/res_varied_alpha.RData")


# Lambda ------------------------------------------------------------------

lambdas <- seq(from = 0, to = 0.8, by = 0.2)
lambda_input <- list_modify(baseline, Alpha = NULL, Lambda = lambdas)
alphas_manually_optimized <- c(0.25, 0.02, 1 * 10^-6, 1 * 10^-4)


meta_res_lambda <- map2(.x = features_extraction_methods,
								.y = alphas_manually_optimized,
								.f = vary_parameter,
								variable_specs = lambda_input,
								static_specs = static_specs,
								runs = 4)



names(meta_res_lambda) <- features_extraction_methods


save(meta_res_lambda, file = "simulation_results/res_varied_lambda.RData")



# Delta -------------------------------------------------------------------

deltas <- c(0L, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
delta_input <- list_modify(baseline, Alpha = c(0.1, 0.001, 0.000001, 0.00001),
									Delta = deltas)


meta_res_delta <- map2(.x = features_extraction_methods,
								.y = alphas_manually_optimized,
								.f = vary_parameter,
								variable_specs = delta_input,
								static_specs = static_specs,
								runs = 4)



names(meta_res_delta) <- features_extraction_methods


save(meta_res_delta, file = "simulation_results/res_varied_delta.RData")