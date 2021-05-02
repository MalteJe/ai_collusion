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



t_grouping <- 50000

experiment_job <- "Alpha_final"
(path <- str_c("simulation_results/", experiment_job, "/"))
filenames <- list.files(path) %>%
	str_subset("RData$") %>%
	str_subset("aggregated", negate = TRUE); print(head(filenames))

adjusted_files <- str_replace(filenames, "poly_tiling", "poly-tiling")  %>%
	str_replace("poly_separate", "poly-separate") %>%
	str_sub(end = -7L)

meta_overview <- tibble(filename = adjusted_files) %>%
	separate(col = filename, into = c("feature_method", "varied_parameter", "run_id"), sep = "_") %>%
	mutate(path = str_c(path, filenames), successful = TRUE)

meta_overview %>%
	group_by(feature_method, varied_parameter) %>%
	count() %>%
	arrange(n)


# meta_overview <- tibble(filename = adjusted_files) %>%
# 	separate(col = filename, into = c("feature_method", "varied_parameter", "run_id"), sep = "_") %>%
# 	mutate(path = str_c(path, filenames), successful = TRUE) %>%
# 	complete(feature_method, varied_parameter, run_id, fill = list(successful = FALSE))

(variations <- sort(unique(as.numeric(meta_overview$varied_parameter)), decreasing = TRUE))

data_nested <- meta_overview %>%
	mutate(sim = map(path, load_return, t_grouping = t_grouping)) %>%
	select(-path)


save(data_nested, file = str_c(path, "aggregated.RData"))



# Graphs ------------------------------------------------------------------

data <- data_nested %>%
	unnest_wider(sim) %>%
	mutate(feature_method = fct_relevel(feature_method, "tabular", "tiling", "poly-separated", "poly-tiling"))

theme_tq

ai_theme <- theme(
	axis.title = element_text(size = 14, margin = margin(0.3, 0.3, -1, 0, unit = "mm")),
	axis.text = element_text(size = 11),
	legend.title = element_blank(),
	legend.key = element_rect(fill = "white", color = NA),
	legend.position = "bottom",
	panel.background = element_rect(fill = "white", color = NA),
	panel.border = element_rect(fill = NA, size = 0.2),
	# panel.spacing = unit(4.75, "cm"),
	# panel.border = element_blank(),
	panel.grid.major.x = element_line(color = "grey85", size = 0.2),
	panel.grid.major.y = element_line(color = NA),
	panel.grid.minor.x = element_line(color = NA),
	panel.grid.minor.y = element_line(color = NA),
	plot.margin = margin(1, 0.5,0.5,0.5, "cm"),
	legend.margin = margin(0, 0,0,0, "mm"),
	legend.box.margin = unit(rep(0, 4), "mm")
)


color_dictionary <- c(
	"converged" = "palegreen",
	"not_converged" = "gray",
	"failed" = "tomato4",
	"tabular" = "black",
	"tiling" = "#4DBBD5",
	"poly-tiling" = "#E64B35",
	"poly-separated" = "#F39B7FFF"
)

fill_dictionary <- scale_fill_manual(values = color_dictionary)
color_dictionary <- scale_color_manual(values = color_dictionary)


# data %>%
# 	mutate(converged = (convergence < 500000)) %>%
# 	group_by(feature_method, varied_parameter) %>%
# 	summarize(converged_fraction = mean(converged)) %>%
# 	ggplot(aes(x = varied_parameter, y = converged_fraction, fill = feature_method)) +
# 	geom_col(position = "dodge") +
# 	# facet_wrap(~feature_method) +
# 	# coord_flip() +
# 	theme_tq()
	
	
convergence_info <- data %>%
	mutate(converged = (convergence < 500000)) %>%
	complete(feature_method, varied_parameter) %>%
	filter(!(feature_method == "tabular" & varied_parameter %in% c("1e-06", "1e-08", "1e-10", "1e-12"))) %>%
	group_by(feature_method, varied_parameter) %>%
	summarize(failed = 48 - n(),
				 converged = sum(converged)) %>%
	ungroup() %>%
	mutate(not_converged = 48 - failed - converged) %>%
	pivot_longer(cols = c("converged", "not_converged" ,"failed"), names_to = "status") %>%
	mutate(status = forcats::fct_rev(status))


convergence_info %>%
	ggplot(aes(x = as_factor(as.numeric(varied_parameter)), y = value, fill = status)) +
	geom_col(position = "stack") +
	facet_wrap(~feature_method) +
	theme_tq() +
	fill_dictionary +
	coord_flip() +
	labs(y = "runs", x = expression(alpha)) +
	ai_theme +
	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
ggsave("report/plots/converged.png", width = 25, height = 15, units = "cm")


data %>%
	filter(convergence < 500000) %>%
	#group_by(feature_method) %>%
	ggplot(aes(x = convergence, y = stat(density), col = feature_method)) +
	# geom_freqpoly() +
	geom_freqpoly(size = 1, binwidth = 10000) +
	scale_x_continuous(limits = c(0, 500000), labels = scales::comma) +
	theme_tq() +
	color_dictionary +
	labs(x = "t") +
	ai_theme
ggsave("report/plots/convergence_at.png", width = 25, height = 15, units = "cm")	
	
	









# Vary Alpha --------------------------------------------------------------


data %>%
	group_by(feature_method, varied_parameter) %>%
	summarize(avg_profits = mean(avg_profits, na.rm = TRUE)) %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	ggplot(aes(x = as.double(varied_parameter), y = Delta, col = feature_method)) +
	geom_line(size = 1) +
	geom_point( size = 3) +
	geom_hline(yintercept = c(0, 1)) +
	theme_tq() +
	scale_x_log10() +
	labs(x = expression(alpha), y = expression(Delta)) +
	color_dictionary

filter(data, !is.na(avg_profits)) %>%
	group_by(feature_method, varied_parameter) %>%
	summarize(avg_profits = mean(avg_profits)) %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	filter(Delta > 0) %>%
	ggplot(aes(x = as.double(varied_parameter), y = Delta, col = feature_method)) +
	geom_hline(yintercept = c(0, 1)) +
	geom_line(size = 1) +
	geom_point( size = 3) +
	scale_y_continuous(expand = c(0, 0), breaks = seq(0,1, by = 0.25), labels = c(expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	# theme_tq() +
	scale_x_log10(minor_breaks = variations, breaks = variations) +
	labs(x = expression(alpha), y = expression(Delta)) +
	ai_theme +
	color_dictionary +
	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
ggsave("report/plots/alpha.png", width = 25, height = 15, units = "cm")


filter(data, !is.na(avg_profits)) %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	filter(Delta > 0) %>%
	ggplot(aes(x = as.factor(varied_parameter), y = Delta, fill = feature_method)) +
	geom_boxplot(position = "dodge") +
	geom_hline(yintercept = c(0, 1)) +
	scale_y_continuous(expand = c(0, 0), breaks = seq(0,1, by = 0.25), labels = c(expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	# theme_tq() +
	# scale_x_continuous(minor_breaks = variations, breaks = variations) +
	xlab(experiment_job) +
	labs(x = expression(alpha), y = expression(Delta)) +
	ai_theme


# Learning Phase Trajectory  ----------------------------------------------------------

# experiment_ids <- 8; variations[experiment_ids]
# 
# 
# 
# outcomes <- data %>%
# 	select(-intervention, -avg_profits) %>%
# 	unnest(outcomes) %>%
# 	filter(!is.na(t_group))

manually_optimized_alpha <- data %>%
	filter(
		(feature_method == "poly-separated" & varied_parameter == "1e-06") |
		(feature_method == "poly-tiling" & varied_parameter == "1e-12") |
		(feature_method == "tiling" & varied_parameter == "0.001")  |
		(feature_method == "tabular" & varied_parameter == "0.1")
) 

outcomes <- manually_optimized_alpha %>%
	select(-intervention, -avg_profits) %>%
	unnest(outcomes)




learning_phase <- outcomes %>%
	# complete(feature_method, varied_parameter, run_id, t_group) %>%
	# group_by(feature_method, varied_parameter, run_id) %>%
	# fill(price, Delta, .direction = "down") %>%
	pivot_longer(cols = c(price, Delta), names_to = "metric") # %>%
	# filter(varied_parameter %in% variations[experiment_ids])

learning_phase %>%
	mutate(p_n = ifelse(metric == "Delta", 0, 1.47),
			 p_m = ifelse(metric == "Delta", 1, 1.93 )) %>%
	ggplot(aes(x = t_group * t_grouping, y = value, group = interaction(feature_method, run_id, varied_parameter), col = feature_method)) +
	geom_hline(aes(yintercept = p_n), linetype = "dashed") +	
	geom_hline(aes(yintercept = p_m), linetype = "dashed") +	
	geom_line() +
	facet_wrap(~metric, ncol =1, scales = "free_y") +
	theme_tq() +
	ai_theme +
	scale_x_continuous(labels = scales::comma) +
	color_dictionary +
	labs(x = "t", y = " ")
ggsave("report/plots/all_runs.png", width = 25, height = 20, units = "cm")
	

learning_phase %>%
	group_by(feature_method, varied_parameter, t_group, metric) %>%
	summarize(value = mean(value, na.rm = TRUE)) %>%
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
	geom_violin(position = "dodge", scale = "width") +
	# geom_boxplot(position = "dodge") +
	facet_wrap(~metric, ncol = 1, scales = "free_y") +
	theme_tq() +
	fill_dictionary
	

# Intervention



intervention <- manually_optimized_alpha %>%
	select(-outcomes, -avg_profits) %>%
	unnest(intervention)



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
	color_dictionary +
	facet_wrap(~feature_method, nrow = 2)
ggsave("report/plots/average_intervention.png", width = 25, height = 20, units = "cm")

intervention %>%
	filter(tau > -1) %>%
	group_by(feature_method, varied_parameter, run_id) %>%
	transmute(price_change_1 = price_1 - first(price_1),
			 price_change_2 = price_2 - first(price_2),
			 tau = tau) %>%
	pivot_longer(cols = c("price_change_1", "price_change_2"), names_to = "price") %>%
	ggplot(aes(x = as.factor(tau), y = value, fill = feature_method)) +
	geom_violin(scale = "width", trim = TRUE) +
	# geom_boxplot() +
	fill_dictionary +
	facet_grid(feature_method~price) +
	theme_tq() +
	ai_theme +
	labs(x = expression(tau))
ggsave("report/plots/intervention_violin.png", width = 25, height = 25, units = "cm")



