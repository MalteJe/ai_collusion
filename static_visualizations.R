load("simulation_results/Alpha_final/aggregated.RData")


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



