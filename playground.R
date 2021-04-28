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
	str_subset("^(?!.*playground.R$)") %>% 
	str_subset(".R$") %>%
	walk(source)



# Gradient Descent --------------------------------------------------------

library(profvis)

profvis::profvis(
	single_res <- single_run(Algorithm = "expected",
									 n = 2,
									 seed = 1234567,
									 zeta = 1,
									 rounding_precision = 8,
									 m = 19,
									 TT = 20000,
									 TT_intervention = 10,
									 Alpha = 0.00001,
									 Beta = 8*10^-4,
									 Gamma = 0.05,
									 Delta = 0.95,
									 Lambda = 0.5,
									 Epsilon_constant = NA,
									 Psi = 1,
									 w_init = 0,
									 r_adjust = 0.2229272,
									 run_id = NA,
									 specifications = list(
									 	degree = 6,
									 	degree_sep = 5,
									 	degree_poly_tiling = 5,
									 	poly_n_tilings = 5,
									 	poly_n_tiles = 4,
									 	n_tilings = 5,
									 	n_tiles = 10
									 ),
									 features_by = "poly_tiling",								 
									 td_error_method = "discounted",
									 dutch_traces = TRUE,
									 policy = "greedy",
									 convergence_chunk_length = 5000,
									 convergence_cycle_length = 10,
									 convergence_check_frequency = 5000,
									 c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)
)



single_res <- single_run(Algorithm = "expected",
								 n = 2,
								 seed = 1234567,
								 zeta = 1,
								 rounding_precision = 8,
								 m = 19,
								 TT = 1000000,
								 TT_intervention = 10,
								 Alpha = 0.2,
								 Beta = 1*10^-3,
								 Gamma = 0.05,
								 Delta = 0.95,
								 Lambda = 0.5,
								 Epsilon_constant = NA,
								 Psi = 1,
								 w_init = 0,
								 r_adjust = 0.2229272,
								 run_id = NA,
								 specifications = list(
								 	degree = 5,
								 	degree_sep = 5,
								 	degree_poly_tiling = 5,
								 	poly_n_tilings = 5,
								 	poly_n_tiles = 4,
								 	n_tilings = 5,
								 	n_tiles = 10
								 ),
								 features_by = "tiling",								 
								 td_error_method = "discounted",
								 dutch_traces = TRUE,
								 policy = "greedy",
								 convergence_chunk_length = 500,
								 convergence_cycle_length = 10,
								 convergence_check_frequency = 500,
								 c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)



# single_simulation_outcomes <- single_res$outcomes

1 * exp(- 4 * 10^-5 * c(1, 10000, 100000, 300000, 500000))
exp(- Beta * c(1, 10000, 100000, 300000, 500000))

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

