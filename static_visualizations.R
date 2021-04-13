# replace runs with error recoveries with dummy run -----------------------



dummy_run <- list(
	outcomes = matrix(rep(NA_real_, 400), ncol = 4, dimnames = list(NULL, c("price_1", "price_2", "profit_1", "profit_2"))),
	convergence = list(
		converged = TRUE,
		convergence_t = 2),
	run_id = NA,
	specs = list(
		TT_intervention = 10,
		convergence_cycle_length = 1
	)
)



replace_errors <- function(run) {
	if(class(run) == "try-error") {
		return(dummy_run)
	} else {
		return(run)
	}
}

replace_errors_experiment <- function(experiment) {
	map(experiment,
		 replace_errors)
}

replace_errors_feature <- function(features_by) {
	map(features_by,
		 replace_errors_experiment)
}


meta_meta_res <- map(meta_res_alpha,
							replace_errors_feature)



str(meta_meta_res[[1]][[3]][[1]])


# trajectory of "main" specifications -------------------------------------

get_convergence_t <- function(run) {
	if (run$convergence$converged) {
		return(run$convergence$convergence_t)
	} else {
		return(run$specs$TT)
	}
}

get_delta <- function(profit) {
	(profit - 0.2229272) / (0.3374905 - 0.2229272)
}

run_trajectory <- function(run, t_group) {
	
	conv_t <- get_convergence_t(run)
	
	run$outcomes[1:conv_t,] %>%
		as_tibble() %>%
		transmute(t = row_number(),
					 t_group = (t-1) %/% t_group + 1,
					 price = (price_1 + price_2)/2 ,
					 Delta = get_delta((profit_1 + profit_2)/2)) %>%
		group_by(t_group) %>%
		summarize_at(c("price", "Delta"), mean) %>%
		mutate(run_id = run$run_id)
}


experiment_trajectories <- function(feature_res, experiment_id, t_group) {
	experiment <- feature_res[[experiment_id]]
	
	map_dfr(experiment,
		 run_trajectory,
		 t_group) %>%
		complete(t_group, run_id) %>%
		group_by(run_id) %>%
		fill(price, Delta, .direction = "down") %>%
		pivot_longer(cols = c(price, Delta), names_to = "metric")
}

# experiment_trajectories(meta_meta_res[[1]], 2, t_group = 1000) %>%
# 	ggplot(aes(x = t_group * 1000, y = value, col = as.factor(run_id))) +
# 	geom_line() +
# 	facet_wrap(~metric, ncol = 1, scales = "free_y") +
# 	theme_tq()

average_trajectories <- function(feature_res, experiment_id, t_group) {
	experiment_trajectories(feature_res, experiment_id, t_group) %>%
		group_by(t_group, metric) %>%
		summarize(value = mean(value))
}






map_dfr(meta_meta_res,
	 average_trajectories,
	 experiment_id = 2,
	 t_group = 1000, .id = "feature_method") %>%
	ungroup() %>%
	complete(feature_method, t_group, metric) %>%
	group_by(feature_method, metric) %>%
	fill(value, .direction = "down") %>%
	mutate(p_n = ifelse(metric == "Delta", 0, 1.47),
			 p_m = ifelse(metric == "Delta", 1, 1.97 )) %>%
	ggplot(aes(x = t_group * 1000, y = value, color = feature_method)) +
	geom_hline(aes(yintercept = p_n)) +	
	geom_hline(aes(yintercept = p_m)) +	
	geom_line(size = 1) +
		facet_wrap(~ metric, ncol = 1, scales = "free_y") +
		theme_tq()


# experiment_trajectories(meta_meta_res[[1]], 2, t_group = 10000) %>%
# 	ggplot(aes(x = as.factor(t_group * 10000), y = value)) +
# 	geom_boxplot() +
# 	facet_wrap(~metric, ncol = 1, scales = "free_y")

map_dfr(meta_meta_res, experiment_trajectories, 3, t_group = 10000, .id = "feature_method") %>%
	ggplot(aes(x = as.factor(t_group * 10000), y = value, fill = feature_method)) +
	geom_boxplot(position = "dodge") +
	facet_wrap(~metric, ncol = 1, scales = "free_y") +
	theme_tq()






# Intervention ------------------------------------------------------------


intervention_prices <- function(run, t_before_intervention) {
	
	tail(run$outcomes, run$specs$TT_intervention + t_before_intervention) %>%
		as_tibble() %>%
		select(price_1, price_2) %>%
		mutate(run_id = run$run_id,
				 tau = row_number() - t_before_intervention + 1)
}

intervention_experiment <- function(feature_res, experiment_id, t_before_intervention) {
	map_dfr(feature_res[[experiment_id]],
			  .f = intervention_prices,
			  t_before_intervention = t_before_intervention)
}


intervention_avg_prices <- function(feature_res, experiment_id, t_before_intervention) {
	intervention_experiment(feature_res, experiment_id, t_before_intervention) %>%
		group_by(tau) %>%
		summarize(price_1 = mean(price_1),
					 price_2 = mean(price_2)) %>%
		pivot_longer(cols = c("price_1", "price_2"), names_to = "price")
}


map_dfr(meta_meta_res,
	 .f = intervention_avg_prices,
	 experiment_id = 2,
	 t_before_intervention = 8,
	 .id = "feature_method") %>%
	ggplot(aes(x = tau, y = value, linetype = price, shape = price, col = feature_method)) +
		geom_hline(yintercept = c(1.47, 1.97)) +
		geom_vline(xintercept = 0, linetype = 2) +
		geom_point(size = 3) +
		geom_line(size = 1) +
		theme_tq() +
		facet_wrap(~feature_method)

# intervention_avg_prices(meta_meta_res[[1]], 5, 10) %>%
# 	ggplot(aes(x = tau, y = value, linetype = price, shape = price)) +
# 	geom_hline(yintercept = c(1.47, 1.97)) +
# 	geom_point(size = 3) +
# 	geom_line(size = 1) +
# 	theme_tq()



map_dfr(meta_meta_res,
		 	.f = intervention_experiment,
		 	experiment_id = 2,
		 	t_before_intervention = 2, .id = "feature_method") %>%
	group_by(feature_method, run_id) %>%
	mutate(price_change_1 = price_1 - first(price_1),
			 price_change_2 = price_2 - first(price_2)) %>%
	pivot_longer(cols = c("price_change_1", "price_change_2"), names_to = "price") %>%
	ggplot(aes(x = as.factor(tau), y = value)) +
	geom_boxplot() +
	facet_grid(feature_method~price)








# varying Alpha -----------------------------------------------------------


avg_profits_upon_convergence <- function(run) {
	conv_t <- get_convergence_t(run)
	
	# calculate average profits of maximally considered cycle length at convergence (before intervention)
	run$outcomes[(conv_t - run$specs$convergence_cycle_length):conv_t,3:4] %>% mean()
}




experiment_avg_profits <- function(experiment_res) {
	map_dbl(experiment_res, avg_profits_upon_convergence) %>% mean()
}


# alpha_avg_profits <- map_dbl(.x = meta_meta_res[[4]],
# 										.f = experiment_avg_profits)


feature_avg_profits <- function(feature_res) {
	 map_dbl(.x = feature_res,
	 		  .f = experiment_avg_profits)
}


# feature_avg_profits(meta_meta_res[[1]])


avg_profits_varied_alpha <- map_dfc(.x = meta_meta_res,
												.f = feature_avg_profits) %>%
	mutate(alpha = alphas) %>%
	pivot_longer(cols = features_extraction_methods, names_to = "feature_method", values_to = "avg_profit") %>%
	mutate(Delta = get_delta(avg_profit))


ggplot(avg_profits_varied_alpha, aes(x = alpha, y = Delta, col = feature_method)) +
	geom_line(size = 1) +
	geom_point( size = 3) +
	geom_hline(yintercept = c(0, 1)) +
	theme_tq() +
	scale_x_log10()



# Varying Lambda ----------------------------------------------------------

avg_profits_varied_lambda <- map_dfc(.x = meta_res_lambda,
												 .f = feature_avg_profits) %>%
	mutate(lambda = lambdas) %>%
	pivot_longer(cols = features_extraction_methods, names_to = "feature_method", values_to = "avg_profit") %>%
	mutate(Delta = get_delta(avg_profit))


ggplot(avg_profits_varied_lambda, aes(x = lambda, y = Delta, col = feature_method)) +
	geom_line(size = 1) +
	geom_point( size = 3) +
	geom_hline(yintercept = c(0, 1)) +
	theme_tq()
