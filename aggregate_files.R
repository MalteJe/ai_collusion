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

load_return <- function(path) {
	if(is.na(path)) {
		return(list(
			outcomes = NA,
			convergence = NA
		))
	} else {
		load(path)
		
		# browser()
		conv_t <- get_convergence_t(res)
		
		
		return(list(
			outcomes = as_tibble(res$outcomes),
			convergence = conv_t))
		
		rm(res)
		gc()
	}
}

# specify path, list single runs in directory and adjust names

experiment_job <- "Alpha_10h"
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

(variations <- unique(meta_overview$varied_parameter))

data_nested <- meta_overview %>%
	mutate(sim = map(path, load_return)) %>%
	select(-path)

data_nested %>%
	group_by(feature_method, varied_parameter) %>%
	summarize(finished_runs_perc = mean(successful)) %>%
	arrange(finished_runs_perc)

data <- data_nested %>%
	unnest_wider(sim) %>%
	unnest(outcomes) %>%
	group_by(feature_method, varied_parameter, run_id) %>%
	mutate(t = row_number())


# Learning Phase Trajectory  ----------------------------------------------------------

t_grouping <- 10000
experiment_ids <- 1

learning_phase <- data %>%
	filter(t <= 100000) %>%
	group_by( feature_method, varied_parameter, run_id) %>%
	transmute(t = t,
				 t_group = (t-1) %/% t_grouping + 1,
				 price = (price_1 + price_2)/2,
				 Delta = get_delta((profit_1 + profit_2)/2),
				 convergence = convergence) %>%
	filter(t <= convergence) %>%
	group_by(feature_method, varied_parameter, run_id, t_group) %>%
	summarize_at(c("price", "Delta"), mean) %>%
	ungroup() %>%
	complete(feature_method, varied_parameter, run_id, t_group) %>%
	group_by(feature_method, varied_parameter, run_id) %>%
	fill(price, Delta, .direction = "down") %>%
	pivot_longer(cols = c(price, Delta), names_to = "metric") %>%
	filter(varied_parameter %in% variations[experiment_ids])
	


learning_phase %>%
	ggplot(aes(x = t_group, y = value, group = interaction(feature_method, run_id, varied_parameter), col = feature_method)) +
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
	filter(t > (convergence - 8)) %>%
	mutate(tau = t - convergence) %>%
	filter(varied_parameter %in% variations[experiment_ids])



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
	facet_wrap(~feature_method)
	


intervention %>%
	filter(t > (convergence - 1)) %>%
	group_by(feature_method, varied_parameter, run_id) %>%
	transmute(price_change_1 = price_1 - first(price_1),
			 price_change_2 = price_2 - first(price_2),
			 tau = tau) %>%
	pivot_longer(cols = c("price_change_1", "price_change_2"), names_to = "price") %>%
	ggplot(aes(x = as.factor(tau), y = value)) +
	geom_boxplot() +
	facet_grid(feature_method~price)




# Vary Alpha --------------------------------------------------------------

data %>%
	filter(t <= convergence, t > convergence - 100) %>%
	mutate(avg_profits = (profit_1 + profit_2) / 2) %>%
	group_by(feature_method, varied_parameter) %>%
	summarize(avg_profit = mean(avg_profits)) %>%
	mutate(Delta = get_delta(avg_profit)) %>%
	ggplot(aes(x = as.double(varied_parameter), y = Delta, col = feature_method)) +
	geom_line(size = 1) +
	geom_point( size = 3) +
	geom_hline(yintercept = c(0, 1)) +
	theme_tq() +
	scale_x_log10() +
	xlab(experiment_job)
	
				 