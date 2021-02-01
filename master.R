library(purrr)
library(tidyquant)
library(tidyverse)
library(parallel)

# load functions from helper scripts
getwd() %>%
	list.files(full.names = TRUE) %>%
	str_subset("^(?!.*master.R$)") %>% str_subset(".R$") %>%
	walk(source)
	
# Action Space ------------------------------------------------------------

# n <- 2
# zeta <- 0.1
# p_n <- 1.4728
# p_m <- 1.92498
# m <- 15



# function to choose action based on e-greedy policy
select_action_id <- function(Q, action_id, epsilon, m) {
	
	# choose action based on current policy
	# draw from binomial distribution to determine whether to explore or exploit
	if(rbinom(n = 1, size = 1, prob = epsilon)) {
		
		# exploration: choose random action
		selected_action_id <- sample(1:m, 1)
	} else {
		
		# exploitation: choose optimal action
		selected_action_id <- which.max(Q[action_id,])
	}
	return(selected_action_id)
}


# define function for a single run:

single_run <- function(n, zeta, m, delta, TT, alpha, beta, seed = NA, ...) {
	
	# calculate discrete set of prices
	p_n <- nash_prices(n, ...)
	p_m <- optimize_joint_profits(n, ...) %>% mean()
	available_prices <- seq(from = p_n - zeta*(p_m - p_n), to = p_m + zeta * (p_m - p_n), length.out = m)
	
	# calculate reward set as response to cartesian product of available prices
	R <- cross2(available_prices, available_prices) %>%
		map(as.numeric) %>% 
		map(calculate_profits, ...)
	
	# initilaize Q matrix
	initial_Q_values <- map_dbl(1:length(available_prices), init_Q_entry, available_prices = available_prices, delta = delta, ...)
	init_Q <- matrix(rep(initial_Q_values, times = m^2), ncol = m, byrow = TRUE)
	Qs <- list(init_Q, init_Q)
	
	# pre-determine time-declining exploration rate
	epsilon <- exp(-beta * 1:TT)
	
	#Set up trackers
	t <- 1  # tracks time
	outcomes <- matrix(NA, nrow = TT, ncol = n * 2,
							 dimnames = list(NULL, map(c("price", "profit"), ~str_c(., 1:n, sep = "_")) %>% unlist()))
	
	# sample initial actions
	if (!is.na(seed)) {set.seed(seed)}
	initial_actions <- sample(1:m, n, replace = TRUE)
	s_t <- map(0:(n-1), shifter, x = initial_actions) %>%
		map_dbl(get_state_id, m = m)
	
	while (t < TT) {
		
		# select actions from all players based on current policies
		selected_action_ids <- map2_dbl(.x = Qs,
												  .y = s_t,
												  .f = select_action_id,
												  epsilon = epsilon[t],
												  m = m)
		
		# get price according to action's id
		selected_actions <- available_prices[selected_action_ids]
		
		# retrieve profits from list
		r <- R[[selected_action_ids[1] + (selected_action_ids[2]-1) * m]]
		
		# observe next stage status (i.e. collect price choice id's)
		s_t2 <- map(0:(n-1), shifter, x = selected_action_ids) %>%
			map_dbl(get_state_id, m = m)
		
		# update Q-matrix entries
		for (a in seq_along(Qs)) {
			Qs[[a]][s_t[a], selected_action_ids[a]] <- (1-alpha) * Qs[[a]][s_t[a], selected_action_ids[a]] + alpha * (r[a] + delta * max(Qs[[a]][s_t2[a],]))
		}
		
		# record prices and profits
		outcomes[t,] <- c(selected_actions, r)
		
		# print informative message every 100,000 timesteps
		if (t %% 100000 == 0) {print(t)}
		
		# update state & timer
		s_t <- s_t2
		t <- t + 1
	}
	return(list(outcomes = outcomes,Q = Qs, timestamp = Sys.time()))
}



res <- single_run(n = 2,
			  zeta = 0.1,
			  m = 15,
			  delta = 0.95,
			  TT = 120000,
			  alpha = 0.1,
			  beta = 5*10^-6,
			  seed = 1234567,
			  c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)


str(res)
str(res$outcomes)
single_simulation_outcomes <- res$outcomes
single_simulation_outcomes <- meta_res[[4]]$outcomes
dim(single_simulation_outcomes)


tail(single_simulation_outcomes)

metrics <- dimnames(single_simulation_outcomes)[[2]]
as_tibble(single_simulation_outcomes) %>%
	mutate(t = row_number(),
			 t_group = (t-1) %/% 2000 + 1) %>%
	group_by(t_group) %>%
	summarize_at(metrics, mean) %>%
	pivot_longer(cols = metrics, names_to = "type", values_to = "value") %>%
	ggplot(aes(x = t_group, y = value)) +
	geom_line() +
	facet_wrap(~type, nrow = 3, scales = "free_y") +
	theme_tq()



as_tibble(single_simulation_outcomes) %>%
	mutate(t = row_number()) %>%
	filter(t > 1199500) %>%
	pivot_longer(cols = metrics, names_to = "type", values_to = "value") %>%
	ggplot(aes(x = t, y = value)) +
	geom_line() +
	facet_wrap(~type, nrow = 3, scales = "free_y")


(no_of_cores <- detectCores(all.tests = TRUE, logical = TRUE))
local_cluster <- makeCluster(no_of_cores-1)
clusterExport(local_cluster, c("map2_dbl", "%>%", "cross2", "map", "map_dbl", "str_c",
						  as.vector(lsf.str())))


seeds <- 1234567:1234573

meta_res <- parallel::parLapply(cl = local_cluster, X = seeds, fun = single_run,
										  n = 2,
										  zeta = 0.1,
										  m = 15,
										  delta = 0.95,
										  TT = 1200000,
										  alpha = 0.1,
										  beta = 5*10^-6,
										  c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)

stopCluster(local_cluster)

save(meta_res, file = "simulation_results/first.RData")
rm(list = ls())
load("simulation_results/first.RData")
str(meta_res)

# Profiling ---------------------------------------------------------------


library(profvis)
profvis({
	res <- single_run(n = 2,
							zeta = 0.1,
							m = 15,
							delta = 0.95,
							TT = 1200000,
							alpha = 0.1,
							beta = 5*10^-6,
							seed = 1234567,
							c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)
})

