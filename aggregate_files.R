library(tidyverse)
library(janitor)
library(tidyquant)

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


load_return <- function(path, t_grouping, t_before_intervention = 8, t_profit = 100) {
	
	if(is.na(path)) {
		return(list(
			outcomes = NA,
			avg_profits = NA,
			intervention = NA,
			convergence = NA
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
			# Delta = get_delta((profit_1 + profit_2)/2),
			profit = (profit_1 + profit_2)/2
			) %>%
			filter(t <= conv_t) %>%
			group_by(t_group) %>%
			summarize_at(c("price", "profit"), mean) %>%
			mutate(Delta = get_delta(profit)) %>%
			select(-profit)
		
		# browser()
		avg_profits <- tail(res$outcomes, t_profit + res$specs$TT_intervention)[, c("profit_1", "profit_2")] %>%
			head(t_profit) %>%
			mean()
		
		return(list(
			outcomes = outcomes,
			avg_profits = avg_profits,
			intervention = intervention,
			convergence = conv_t))
		
		rm(res)
		rm(outcomes)
		gc()
	}
}


# load_return("simulation_results/comparison_gpc/tiling_0.1_5.RData", t_grouping = 50000)


# specify path, list single runs in directory and adjust names



t_grouping <- 5000

experiment_job <- "Alpha_home"
(path <- str_c("simulation_results/", experiment_job, "/"))
filenames <- list.files(path) %>%
	str_subset("RData$"); print(head(filenames))

adjusted_files <- str_replace(filenames, "poly_tiling", "poly-tiling")  %>%
	str_replace("poly_separate", "poly-separate") %>%
	str_sub(end = -7L)

meta_overview <- tibble(filename = adjusted_files) %>%
	separate(col = filename, into = c("feature_method", "varied_parameter", "run_id"), sep = "_") %>%
	mutate(path = str_c(path, filenames), successful = TRUE) %>%
	complete(feature_method, varied_parameter, run_id, fill = list(successful = FALSE))

(variations <- unique(as.numeric(meta_overview$varied_parameter)))

data_nested <- meta_overview %>%
	mutate(sim = map(path, load_return, t_grouping = t_grouping)) %>%
	select(-path)


data_nested %>%
	group_by(feature_method, varied_parameter) %>%
	summarize(finished_runs_perc = mean(successful)) %>%
	arrange(finished_runs_perc)


data <- data_nested %>%
	unnest_wider(sim)


# Vary Alpha --------------------------------------------------------------

data %>%
	group_by(feature_method, varied_parameter) %>%
	summarize(avg_profits = mean(avg_profits)) %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	ggplot(aes(x = as.double(varied_parameter), y = Delta, col = feature_method)) +
	geom_line(size = 1) +
	geom_point( size = 3) +
	geom_hline(yintercept = c(0, 1)) +
	theme_tq() +
	scale_x_log10() +
	xlab(experiment_job)




# Learning Phase Trajectory  ----------------------------------------------------------

experiment_ids <- 1


outcomes <- data %>%
	select(-intervention, -avg_profits) %>%
	unnest(outcomes) %>%
	filter(!is.na(t_group))


learning_phase <- outcomes %>%
	complete(feature_method, varied_parameter, run_id, t_group) %>%
	group_by(feature_method, varied_parameter, run_id) %>%
	fill(price, Delta, .direction = "down") %>%
	pivot_longer(cols = c(price, Delta), names_to = "metric") %>%
	filter(varied_parameter %in% variations[experiment_ids])

learning_phase %>%
	ggplot(aes(x = t_group * t_grouping, y = value, group = interaction(feature_method, run_id, varied_parameter), col = feature_method)) +
	geom_line() +
	facet_wrap(~metric, ncol =1, scales = "free_y") +
	theme_tq()

learning_phase %>%
	group_by(feature_method, varied_parameter, t_group, metric) %>%
	summarize(value = mean(value)) %>%
	mutate(p_n = ifelse(metric == "Delta", 0, 1.47),
			 p_m = ifelse(metric == "Delta", 1, 1.93 )) %>%
	ggplot(aes(x = t_group * t_grouping, y = value, color = feature_method)) +
	geom_hline(aes(yintercept = p_n)) +	
	geom_hline(aes(yintercept = p_m)) +	
	geom_line(size = 1) +
	facet_wrap(~ metric, ncol = 1, scales = "free_y") +
	theme_tq()


learning_phase %>%
	ggplot(aes(x = as.factor(t_group * t_grouping), y = value, fill = feature_method)) +
	geom_boxplot(position = "dodge") +
	facet_wrap(~metric, ncol = 1, scales = "free_y") +
	theme_tq()
	

# Intervention



intervention <- data %>%
	select(-outcomes, -avg_profits) %>%
	unnest(intervention) %>%
	filter(varied_parameter %in% variations[experiment_ids], !is.na(price_1))



intervention %>%
	group_by(feature_method, varied_parameter, tau) %>%
	summarize(price_1 = mean(price_1),
				 price_2 = mean(price_2)) %>%
	pivot_longer(cols = c("price_1", "price_2"), names_to = "price") %>%
	ggplot(aes(x = tau, y = value, linetype = price, shape = price, col = feature_method)) +
	geom_hline(yintercept = c(1.47, 1.93)) +
	geom_vline(xintercept = 0, linetype = 2) +
	geom_point(size = 3) +
	geom_line(size = 1) +
	theme_tq() +
	facet_wrap(~feature_method, nrow = 2)
	

intervention %>%
	filter(tau > -1) %>%
	group_by(feature_method, varied_parameter, run_id) %>%
	transmute(price_change_1 = price_1 - first(price_1),
			 price_change_2 = price_2 - first(price_2),
			 tau = tau) %>%
	pivot_longer(cols = c("price_change_1", "price_change_2"), names_to = "price") %>%
	ggplot(aes(x = as.factor(tau), y = value)) +
	geom_boxplot() +
	facet_grid(feature_method~price)




