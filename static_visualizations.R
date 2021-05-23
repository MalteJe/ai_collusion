library(tidyverse)
library(tidyquant)
library(rlang)
library(ggbeeswarm)
library(xtable)

source("economic_environment.R")


# Theme ------------------------------------------------------------------
# visual specifications
theme_tq

ai_theme <- theme(
	axis.title = element_text(size = 14, margin = margin(0.3, 0.3, -1, 0, unit = "mm")),
	axis.text = element_text(size = 11),
	panel.background = element_rect(fill = "white", color = NA),
	panel.border = element_rect(fill = NA, size = 0.2),
	# panel.spacing = unit(4.75, "cm"),
	# panel.border = element_blank(),
	panel.grid.major.x = element_line(color = "grey85", size = 0.2),
	panel.grid.major.y = element_line(color = NA),
	panel.grid.minor.x = element_line(color = NA),
	panel.grid.minor.y = element_line(color = NA),
	plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
	# legend.spacing = margin(10, 10, 10, 10, "cm"),
	legend.margin = margin(0, 0,0,0, "mm"),
	legend.title = element_blank(),
	legend.text = element_text(size = rel(1.1), margin = margin(r = 15, unit = "pt")),
	legend.key = element_rect(fill = "white", color = NA),
	legend.position = "bottom",
	legend.direction = "horizontal",
	legend.box = "vertical",
	legend.box.margin = unit(rep(0, 4), "mm"),
	strip.background = element_rect(fill = "#2c3e50", color = "#2c3e50", size = rel(1.3)), 
	strip.text = element_text(color = "grey90", face = "bold", size = rel(1.1)),
)

# consistently map factors to colors and apply to fill and color scales
color_dictionary <- c(
	"converged" = "palegreen",
	"not converged" = "gray",
	"failed" = "tomato4",
	"tabular" = "black",
	"tiling" = "#4DBBD5",
	"poly-tiling" = "#E64B35",
	"poly-separated" = "#F39B7FFF"
)

fill_dictionary <- scale_fill_manual(values = color_dictionary)
color_dictionary <- scale_color_manual(values = color_dictionary)


# function to line & scatter-plot with Delta on the y-axis as a function of feature extraction method and specified x-axis
point_line_plot <- function(data, varied, filter_cond = "TRUE", x_lab, x_log10 = FALSE) {
	
	
	variations <- sort(unique(data[eval(varied)] %>%
									  	pull(varied) %>%
									  	as.numeric()))

	res <- filter(data, !is.na(avg_profits)) %>%
		group_by_at(vars(all_of(c("feature_method", varied)))) %>%
		summarize(avg_profits = mean(avg_profits)) %>%
		mutate(Delta = get_delta(avg_profits)) %>%
		filter(!! parse_expr(filter_cond))
	
	out <- res %>%
		ggplot(aes(x = as.double(!!sym(varied)), y = Delta, col = feature_method)) +
			geom_hline(yintercept = c(0, 1)) +
			geom_line(size = 1) +
			geom_point( size = 3) +
			scale_y_continuous(expand = c(0, 0), breaks = seq(0,1, by = 0.25),
									 labels = c(expression(Delta[n]), "0.25", "0.5", "0.75",
									 			  expression(Delta[m]))) +
			labs(x = x_lab, y = expression(Delta)) +
			ai_theme +
			color_dictionary +
			theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
	
	if (x_log10) {
		out <- out + scale_x_log10(minor_breaks = variations, breaks = variations)
	} else {
		out <- out + scale_x_continuous(minor_breaks = variations, breaks = variations)
	}
	print(out)
	return(res)
}


# function to get stacked bar plot with varied parameter on y axis and a count of converged/non-converged/failed runs
convergence_plot <- function(data, varied, filter_cond = "TRUE", runs_per_experiment, x_lab) {
	
	res <- data %>%
		# identify converged runs and 'complete' with implicitly missing runs (without files in folder)
		mutate(converged = (convergence < 500000)) %>%
		complete(feature_method, varied_parameter) %>%
		
		# filter anything out (relevant depending on experiment)
		filter(!! parse_expr(filter_cond)) %>%
		
		# summarize in converged, not-converged and failed runs & wrangle in long format 
		group_by(feature_method, varied_parameter) %>%
		summarize(failed = runs_per_experiment - n_distinct(run_id, na.rm = TRUE),
					 converged = sum(converged)) %>%
		ungroup() %>%
		mutate(`not converged` = runs_per_experiment - failed - converged) %>%
		pivot_longer(cols = c("converged", "not converged" ,"failed"), names_to = "status") %>%
		mutate(status = forcats::fct_rev(status))
	
	out <- res %>%
		ggplot(aes(x = as_factor(as.numeric(!!sym(varied))), y = value, fill = status)) +
			geom_col(position = "stack") +
			facet_wrap(~feature_method) +	fill_dictionary +
			coord_flip() +
			labs(y = "runs", x = x_lab) +
			ai_theme +
			theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
	
	print(out)
	
	return(res)
}




# function to unnest deviation data and plot average prices around deviation of both players
deviation_plot <- function(data, varied, filter_cond = "TRUE", tau_min = -5) {
	
	res <- data  %>%
		filter(!! parse_expr(filter_cond)) %>%
		select(-outcomes, -successful, -varied_parameter_fct, -convergence, -avg_profits) %>%
		unnest(intervention)
	
	out <- res %>%
		group_by(feature_method, varied_parameter, tau) %>%
		summarize("deviating agent" = mean(price_1),
					 "non deviating agent" = mean(price_2)) %>%
		pivot_longer(cols = c("deviating agent", "non deviating agent"), names_to = "price") %>%
		filter(tau > tau_min) %>%
		ggplot(aes(x = tau, y = value, linetype = price, shape = price, col = feature_method)) +
		geom_hline(yintercept = c(1.47, 1.93), linetype = "dashed") +
		geom_vline(xintercept = 0, linetype = "dotted") +
		geom_point(size = 3) +
		geom_line(size = 1) +
		scale_linetype_manual(values = c("solid", "dotted")) +
		facet_wrap(~feature_method, nrow = 2) +
		labs(x = expression(tau), y = "") +
		ai_theme +
		color_dictionary
	print(out)
	
	return(res)
}





















# Convergence --------------------------------------------------------
load("simulation_results/Alpha_final/aggregated.RData")

# invoke function from above to create stacked plot with convergence info
convergence_info <- convergence_plot(data = data,
												 varied = "varied_parameter",
												 filter_cond = "!(feature_method == 'tabular' & varied_parameter %in% c('1e-06', '1e-07', '1e-08', '1e-10', '1e-12')) &
					  !(feature_method == 'tiling' & varied_parameter == '1e-12')",
												 runs_per_experiment = 48,
												 x_lab = expression(alpha))
ggsave("report/plots/converged.png", width = 25, height = 15, units = "cm")

# convergence_info <- data %>%
# 
# # identify converged runs and 'complete' with implicitly missing runs (without files in folder)
# 	mutate(converged = (convergence < 500000)) %>%
# 	complete(feature_method, varied_parameter) %>%
# 
# # these experiments were not attempted and shouldn't appear in the graph as 'failed'
# 	filter(!(feature_method == "tabular" & varied_parameter %in% c("1e-06", "1e-07", "1e-08", "1e-10", "1e-12")) &
# 			 !(feature_method == "tiling" & varied_parameter == "1e-12")) %>%
# 
# # summarize in converged, not-converged and failed runs & wrangle in long format 
# 	group_by(feature_method, varied_parameter) %>%
# 	summarize(failed = 48 - n_distinct(run_id, na.rm = TRUE),
# 				 converged = sum(converged)) %>%
# 	ungroup() %>%
# 	mutate(`not converged` = 48 - failed - converged) %>%
# 	pivot_longer(cols = c("converged", "not converged" ,"failed"), names_to = "status") %>%
# 	mutate(status = forcats::fct_rev(status))

# Bar chart of convergence proportions
# convergence_info %>%
# 	ggplot(aes(x = as_factor(as.numeric(varied_parameter)), y = value, fill = status)) +
# 	geom_col(position = "stack") +
# 	facet_wrap(~feature_method) +	fill_dictionary +
# 	coord_flip() +
# 	labs(y = "runs", x = expression(alpha)) +
# 	ai_theme +
# 	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
# ggsave("report/plots/converged.png", width = 25, height = 15, units = "cm")

convergence_info %>%
	count(status, wt = value) %>%
	filter(status != "failed") %>%
	mutate(prop = n/sum(n))

convergence_info %>%
	group_by(feature_method) %>%
	count(status, wt = value) %>%
	filter(status != "failed") %>%
	mutate(prop = n/sum(n))

data %>%
	filter(convergence < 500000) %>%
	ggplot(aes(x = convergence, y = stat(density), col = feature_method)) +
	geom_freqpoly(size = 1.5, binwidth = 8000, alpha = 0.7) +
	scale_x_continuous(limits = c(0, 500000), labels = scales::comma) +
	color_dictionary +
	labs(x = "t") +
	ai_theme +
	guides(colour = guide_legend(override.aes = list(alpha = 1)))
ggsave("report/plots/convergence_at.png", width = 25, height = 15, units = "cm")	



# Price Cycles ------------------------------------------------------------

data %>%
	ggplot(aes(x = as.factor(cycle_length), fill = feature_method)) +
	geom_bar(position = "stack", na.rm = TRUE) +
	scale_x_discrete(na.translate = FALSE) +
	ai_theme +
	fill_dictionary +
	labs(x = "cycle length", y = "")
ggsave("report/plots/cycle_length.png", width = 25, height = 15, units = "cm")


data %>%
	group_by(feature_method) %>%
	count(cycle_length) %>%
	filter(
		(feature_method == "poly-separated") |
		(feature_method == "tabular" & cycle_length == 10) |
		(feature_method == "tiling" & cycle_length == 2)
	)


# Vary Alpha --------------------------------------------------------------


alpha_delta <- point_line_plot(data = data, varied = "varied_parameter", filter_cond = "Delta >= 0", x_lab = expression(alpha), x_log10 = TRUE)
ggsave("report/plots/alpha.png", width = 25, height = 15, units = "cm")

alpha_delta %>%
	group_by(feature_method) %>%
	summarize(max = max(Delta))

data %>%
	filter(feature_method == "poly-tiling", varied_parameter == "1e-04") %>%
	summarize(avg_profits = mean(avg_profits)) %>%
	mutate(Delta = get_delta(avg_profits))


all_runs <- filter(data, !is.na(avg_profits)) %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	filter(!(feature_method == "poly-tiling" & varied_parameter == "1e-04"))

all_runs %>%	
	ggplot(aes(x = fct_rev(varied_parameter_fct), y = Delta, fill = feature_method)) +
	geom_hline(yintercept = c(0, 1), linetype = "dashed") +
	scale_y_continuous(expand = c(0, 0), breaks = seq(-0.5,1, by = 0.25), labels = c("-0.5", "-0,25", expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	geom_violin(draw_quantiles = 0.5, color = "grey35", scale = "width") +
	labs(x = expression(alpha), y = expression(Delta)) +
	ai_theme +
	facet_wrap(~feature_method) +
	fill_dictionary
ggsave("report/plots/alpha_violin.png", width = 25, height = 15, units = "cm")


poly_sep <- filter(all_runs, feature_method == "poly-separated")

poly_sep %>%
	count(sub_nash = (Delta <= 0)) %>%
	mutate(perc = n / sum(n))
	
poly_sep %>%
	group_by(varied_parameter) %>%
	count(sub_nash = (Delta <= 0)) %>%
	mutate(perc = n / sum(n)) %>%
	arrange(sub_nash, perc)


# Learning Phase Trajectory  ----------------------------------------------------------

t_grouping <- 50000

manually_optimized_alpha <- data %>%
	filter(
		(feature_method == "poly-separated" & varied_parameter == "1e-06") |
			(feature_method == "poly-tiling" & varied_parameter == "1e-08") |
			(feature_method == "tiling" & varied_parameter == "0.001")  |
			(feature_method == "tabular" & varied_parameter == "0.1")
	)


outcomes <- manually_optimized_alpha %>%
	select(-intervention, -avg_profits) %>%
	unnest(outcomes)


learning_phase <- outcomes %>%
	pivot_longer(cols = c(price, Delta), names_to = "metric")

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
	filter(metric == "Delta") %>%
	mutate(t = as_factor(format(t_group * t_grouping, scientific = FALSE))) %>%
	ggplot(aes(x = t, y = value, fill = feature_method)) +
	geom_hline(yintercept = c(0,1), linetype = "dashed") +	
	geom_violin(position = "dodge", scale = "count", draw_quantiles = 0.5) +
	facet_grid(metric~feature_method, scales = "free_y") +
	facet_wrap(~feature_method, nrow = 4, scales = "free_y") +
	theme_tq() +
	labs(y = "") +
	fill_dictionary
ggsave("report/plots/trajectory_Delta.png", width = 25, height = 20, units = "cm")

learning_phase %>%
	filter(metric == "price") %>%
	mutate(t = as_factor(format(t_group * t_grouping, scientific = FALSE))) %>%
	ggplot(aes(x = t, y = value, fill = feature_method)) +
	geom_hline(yintercept = c(1.472927,1.92498), linetype = "dashed") +	
	geom_violin(position = "dodge", scale = "count", draw_quantiles = 0.5) +
	facet_grid(metric~feature_method, scales = "free_y") +
	facet_wrap(~feature_method, nrow = 4, scales = "free_y") +
	theme_tq() +
	labs(y = "") +
	fill_dictionary
ggsave("report/plots/trajectory_price.png", width = 25, height = 20, units = "cm")



# Price Range -------------------------------------------------------------

price_range <- manually_optimized_alpha %>%
	filter(convergence < 500000) %>%
	select(feature_method, varied_parameter, run_id, intervention, cycle_length) %>%
	unnest(intervention) %>%
	filter(tau <= 0) %>%   #zoom in on 'pre-intervention' episodes
	pivot_longer(cols = c(price_1, price_2), names_to = "price") %>%
	group_by(feature_method, run_id, price) %>%
	summarize(min = min(value),
				 max = max(value),
				 range = max - min,
				 cycle_length = unique(cycle_length)) %>%
	group_by(feature_method, cycle_length) %>%
	mutate(n = n())

ggplot(price_range, aes(x = as.factor(cycle_length), y = range, col = feature_method)) +
	geom_hline(yintercept = (1.92498 - 1.472927), linetype = "dashed") +     # reference line: difference between nash and fully collusive prices
	geom_quasirandom(size = 1, width = 0.6, varwidth = TRUE, dodge.width = 0.2) +
	facet_wrap(~feature_method, scales = "free_x") +
	ai_theme +
	fill_dictionary +
	labs(x = "cycle length", y = " ") +
	guides(colour = guide_legend(override.aes = list(size = 2)))
ggsave("report/plots/price_range.png", width = 25, height = 15, units = "cm")



# Intervention ------------------------------------------------------------


intervention <- deviation_plot(data = manually_optimized_alpha, varied = "varied_parameter")
ggsave("report/plots/average_intervention.png", width = 25, height = 20, units = "cm")

# 
# intervention <- manually_optimized_alpha %>%
# 	select(-outcomes, -successful, -varied_parameter_fct, -convergence, -avg_profits) %>%
# 	unnest(intervention)
# 
# 
# 
# intervention %>%
# 	group_by(feature_method, varied_parameter, tau) %>%
# 	summarize("deviating agent" = mean(price_1),
# 				 "non deviating agent" = mean(price_2)) %>%
# 	pivot_longer(cols = c("deviating agent", "non deviating agent"), names_to = "price") %>%
# 	filter(tau > -5) %>%
# 	ggplot(aes(x = tau, y = value, linetype = price, shape = price, col = feature_method)) +
# 	geom_hline(yintercept = c(1.47, 1.93), linetype = "dashed") +
# 	geom_vline(xintercept = 0, linetype = "dotted") +
# 	geom_point(size = 3) +
# 	geom_line(size = 1) +
# 	scale_linetype_manual(values = c("solid", "dotted")) +
# 	facet_wrap(~feature_method, nrow = 2) +
# 	labs(x = expression(tau), y = "") +
# 	ai_theme +
# 	color_dictionary
# ggsave("report/plots/average_intervention.png", width = 25, height = 20, units = "cm")



counterfactual <- intervention %>%
	pivot_longer(cols = c("price_1", "price_2", "profit_1", "profit_2"), names_to = "obs", values_to = "actual") %>%
	group_by(feature_method, run_id, obs) %>%
	mutate(replacement_pos = ifelse(tau <=0, NA, row_number() - cycle_length * (1 + (row_number() - 11) %/% cycle_length))) %>%
	mutate(counterfactual = ifelse(is.na(replacement_pos), actual, actual[replacement_pos]),
			 diff = actual - counterfactual) %>%
	separate(col = obs, into = c("metric", "player"), sep = "_")



full_whisker <- function(x) {
	r <- quantile(x, probs = c(0, 0.15, 0.5, 0.85, 1))
	names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
	r
}


counterfactual %>%
	filter(tau >= 0, metric == "price") %>%
	ggplot(aes(x = as.factor(tau), y = diff, fill = feature_method)) +
	stat_summary(fun.data = full_whisker, geom = "errorbar", width = 0.5) +
	stat_summary(fun.data = full_whisker, geom = "boxplot", col = "gray35") +
	facet_grid(feature_method~player) +
	ai_theme +
	fill_dictionary +
	labs(x = expression(tau), y = "price difference")
ggsave("report/plots/intervention_boxplot.png", width = 25, height = 25, units = "cm")

counterfactual %>%
	filter(tau >= 0, metric == "profit") %>%
	ggplot(aes(x = as.factor(tau), y = diff, fill = feature_method)) +
	stat_summary(fun.data = full_whisker, geom = "errorbar", width = 0.5) +
	stat_summary(fun.data = full_whisker, geom = "boxplot", col = "gray35") +
	facet_grid(feature_method~player) +
	ai_theme +
	fill_dictionary +
	labs(x = expression(tau))
ggsave("report/plots/intervention_profit_boxplot.png", width = 25, height = 25, units = "cm")




deviation_profitability <- counterfactual %>%
	mutate(player = ifelse(player == 1, "deviating agent", "non deviating agent")) %>%
	filter(metric == "profit", tau > 0) %>%
	mutate(actual_discounted = actual * 0.95^(tau - 1),
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%
	group_by(feature_method, run_id, player) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%
	mutate(diff_discounted = actual_discounted - counterfactual_discounted)

deviation_profitability_table <- deviation_profitability %>%
	group_by(feature_method, player) %>%
	summarize("share profitable" = mean(diff_discounted > 0),
				 "share unprofitable" = mean(diff_discounted < 0)); deviation_profitability_table

print(xtable(deviation_profitability_table, type = "latex"),
		file = "report/tables/share_deviation_profitability.tex",
		floating = FALSE,
		include.rownames = FALSE)


ggplot(deviation_profitability, aes(x = diff_discounted, col = feature_method)) +
	geom_freqpoly(size = 1, binwidth = 0.02, alpha = 0.7) +
	facet_wrap(~player, ncol = 1) +
	scale_x_continuous(limits = c(-0.5, 0.5)) +
	ai_theme +
	color_dictionary +
	guides(colour = guide_legend(override.aes = list(alpha = 1))) +
	labs(x = expression(pi), y = "")
ggsave("report/plots/intervention_profitabiliy_polygon.png", width = 25, height = 25, units = "cm")


# not displayed in plot above
filter(deviation_profitability, (diff_discounted <= -0.5 |  diff_discounted >= 0.5))



intervention %>%
	filter(feature_method == "poly-tiling", tau > -3, run_id %in% c(1, 6, 12)) %>%
	pivot_longer(c("price_1", "price_2", "profit_1", "profit_2"), names_to = "temp") %>%
	separate(temp, into = c("metric", "player"), sep = "_") %>%
	mutate(p_n = ifelse(metric == "profit", 0.223, 1.473),
			 p_m = ifelse(metric == "profit", 0.337, 1.925),
			 run_id = str_pad(run_id, 2, pad = "0"),
			 player = ifelse(player == 1, "deviating agent", "non deviating agent")) %>%
	ggplot(aes(x = tau, y = value, shape = player, linetype = player)) +
	geom_hline(aes(yintercept = p_n), linetype = "dashed") +	
	geom_hline(aes(yintercept = p_m), linetype = "dashed") +geom_line() +
	geom_point() +
	facet_grid(metric~run_id, scales = "free_y") +
	scale_x_continuous(labels = scales::comma) +
	ai_theme +
	labs(x  = expression(tau), y = "")
ggsave("report/plots/intervention_poly_tiling.png", width = 25, height = 20, units = "cm")



	
# Prolonged Intervention - TBD --------------------------------------------

load("simulation_results/prolonged_deviation/aggregated.RData")

data %>%
	select(feature_method, varied_parameter, run_id, intervention_prolonged) %>%
	unnest(intervention_prolonged) %>%
	filter(run_id == 1, intervention_length == 10, tau > -4) %>%
	pivot_longer(c("price_1", "price_2", "profit_1", "profit_2"), names_to = "temp") %>%
	separate(temp, into = c("metric", "player"), sep = "_") %>%
	ggplot(aes(x = tau, y = value, col = feature_method, shape = player, linetype = player)) +
	geom_line() +
	geom_point() +
	facet_grid(metric~feature_method, scales = "free_y") +
	ai_theme +
	color_dictionary



# Vary M ------------------------------------------------------------------

load("simulation_results/m_final/aggregated.RData")

data_m <- manually_optimized_alpha %>%
	mutate(varied_parameter = "19",
			 varied_parameter_fct = as.factor("19")) %>%
	bind_rows(data)

# Bar chart of convergence proportions
convergence_info_m <- convergence_plot(data = data_m,
					  varied = "varied_parameter",
					  filter_cond = "varied_parameter != '19'",
					  runs_per_experiment = 16,
					  x_lab = "m")
ggsave("report/plots/converged_m.png", width = 25, height = 15, units = "cm")


# proportions by m (over all feeature extraction methods)
convergence_info_m %>%
	group_by(varied_parameter) %>%
	count(status, wt = value) %>%
	filter(status != "failed") %>%
	mutate(prop = n/sum(n))

# vary m 
m_delta <- point_line_plot(data = data_m, varied = "varied_parameter", x_lab = expression(m), x_log10 = FALSE)
ggsave("report/plots/m.png", width = 25, height = 15, units = "cm")





intervention_m <- data_m %>%
	select(-outcomes, -successful, -varied_parameter_fct, -convergence, -avg_profits) %>%
	unnest(intervention)



intervention_m %>%
	filter(varied_parameter == "10") %>%
	group_by(feature_method, varied_parameter, tau) %>%
	summarize("deviating agent" = mean(price_1),
				 "non deviating agent" = mean(price_2)) %>%
	pivot_longer(cols = c("deviating agent", "non deviating agent"), names_to = "price") %>%
	filter(tau > -5) %>%
	ggplot(aes(x = tau, y = value, linetype = price, shape = price, col = feature_method)) +
	geom_hline(yintercept = c(1.47, 1.93), linetype = "dashed") +
	geom_vline(xintercept = 0, linetype = "dotted") +
	geom_point(size = 3) +
	geom_line(size = 1) +
	scale_linetype_manual(values = c("solid", "dotted")) +
	facet_wrap(~feature_method, nrow = 2) +
	labs(x = expression(tau), y = "") +
	ai_theme +
	color_dictionary
# ggsave("report/plots/average_intervention.png", width = 25, height = 20, units = "cm")


counterfactual_m <- intervention_m %>%
	filter(varied_parameter == "10") %>%
	pivot_longer(cols = c("price_1", "price_2", "profit_1", "profit_2"), names_to = "obs", values_to = "actual") %>%
	group_by(feature_method, run_id, obs) %>%
	mutate(replacement_pos = ifelse(tau <=0, NA, row_number() - cycle_length * (1 + (row_number() - 11) %/% cycle_length))) %>%
	mutate(counterfactual = ifelse(is.na(replacement_pos), actual, actual[replacement_pos]),
			 diff = actual - counterfactual) %>%
	separate(col = obs, into = c("metric", "player"), sep = "_")



counterfactual_m %>%
	filter(tau >= 0, metric == "price") %>%
	ggplot(aes(x = as.factor(tau), y = diff, fill = feature_method)) +
	stat_summary(fun.data = full_whisker, geom = "errorbar", width = 0.5) +
	stat_summary(fun.data = full_whisker, geom = "boxplot", col = "gray35") +
	facet_grid(feature_method~player) +
	ai_theme +
	fill_dictionary +
	labs(x = expression(tau), y = "price difference")
# ggsave("report/plots/intervention_boxplot.png", width = 25, height = 25, units = "cm")




deviation_profitability_m <- counterfactual_m %>%
	mutate(player = ifelse(player == 1, "deviating agent", "non deviating agent")) %>%
	filter(metric == "profit", tau > 0) %>%
	mutate(actual_discounted = actual * 0.95^(tau - 1),
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%
	group_by(feature_method, run_id, player) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%
	mutate(diff_discounted = actual_discounted - counterfactual_discounted)

deviation_profitability_table_m <- deviation_profitability_m %>%
	group_by(feature_method, player) %>%
	summarize("share profitable" = mean(diff_discounted > 0),
				 "share unprofitable" = mean(diff_discounted < 0)); deviation_profitability_table_m

print(xtable(deviation_profitability_table, type = "latex"),
		file = "report/tables/share_deviation_profitability.tex",
		floating = FALSE,
		include.rownames = FALSE)


ggplot(deviation_profitability, aes(x = diff_discounted, col = feature_method)) +
	geom_freqpoly(size = 1, binwidth = 0.02, alpha = 0.7) +
	facet_wrap(~player, ncol = 1) +
	scale_x_continuous(limits = c(-0.5, 0.5)) +
	ai_theme +
	color_dictionary +
	guides(colour = guide_legend(override.aes = list(alpha = 1))) +
	labs(x = expression(pi), y = "")
ggsave("report/plots/intervention_profitabiliy_polygon.png", width = 25, height = 25, units = "cm")




intervention %>%
	filter(feature_method == "poly-tiling", tau > -3, run_id %in% c(1, 6, 12)) %>%
	pivot_longer(c("price_1", "price_2", "profit_1", "profit_2"), names_to = "temp") %>%
	separate(temp, into = c("metric", "player"), sep = "_") %>%
	mutate(p_n = ifelse(metric == "profit", 0.223, 1.473),
			 p_m = ifelse(metric == "profit", 0.337, 1.925),
			 run_id = str_pad(run_id, 2, pad = "0"),
			 player = ifelse(player == 1, "deviating agent", "non deviating agent")) %>%
	ggplot(aes(x = tau, y = value, shape = player, linetype = player)) +
	geom_hline(aes(yintercept = p_n), linetype = "dashed") +	
	geom_hline(aes(yintercept = p_m), linetype = "dashed") +geom_line() +
	geom_point() +
	facet_grid(metric~run_id, scales = "free_y") +
	scale_x_continuous(labels = scales::comma) +
	ai_theme +
	labs(x  = expression(tau), y = "")
ggsave("report/plots/intervention_poly_tiling.png", width = 25, height = 20, units = "cm")






# Vary Lambda -------------------------------------------------------------

load("simulation_results/Lambda_final/aggregated.RData")

# (row-) bind benchmark with optimal alphas with new data
data_lambda <- manually_optimized_alpha %>%
	mutate(varied_parameter = "0.5",
			 varied_parameter_fct = as.factor("0.5")) %>%
	bind_rows(data)


convergence_info <- data_lambda %>%
	mutate(converged = (convergence < 500000)) %>%
	complete(feature_method, varied_parameter) %>%
	filter(!(feature_method == "tabular" & varied_parameter %in% c("1e-06", "1e-07", "1e-08", "1e-10", "1e-12"))) %>%
	filter(!(feature_method == "tiling" & varied_parameter == "1e-12")) %>%
	group_by(feature_method, varied_parameter) %>%
	summarize(failed = 48 - n(),
				 converged = sum(converged)) %>%
	ungroup() %>%
	mutate(`not converged` = 48 - failed - converged) %>%
	pivot_longer(cols = c("converged", "not converged" ,"failed"), names_to = "status") %>%
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
ggsave("report/plots/converged_lambda.png", width = 25, height = 15, units = "cm")


data_lambda %>%
	filter(convergence < 500000) %>%
	ggplot(aes(x = convergence, y = stat(density), col = feature_method)) +
	geom_freqpoly(size = 1, binwidth = 10000) +
	scale_x_continuous(limits = c(0, 500000), labels = scales::comma) +
	theme_tq() +
	color_dictionary +
	labs(x = "t") +
	ai_theme
# ggsave("report/plots/convergence_at.png", width = 25, height = 15, units = "cm")	






variations <- sort(unique(data_lambda$varied_parameter %>% as.numeric()))

filter(data_lambda, !is.na(avg_profits)) %>%
	group_by(feature_method, varied_parameter) %>%
	summarize(avg_profits = mean(avg_profits)) %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	# filter(Delta > 0) %>%
	ggplot(aes(x = as.double(varied_parameter), y = Delta, col = feature_method)) +
	geom_hline(yintercept = c(0, 1)) +
	geom_line(size = 1) +
	geom_point( size = 3) +
	scale_y_continuous(expand = c(0, 0), breaks = seq(0,1, by = 0.25), labels = c(expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	scale_x_continuous(minor_breaks = variations, breaks = variations) +
	labs(x = expression(lambda), y = expression(Delta)) +
	ai_theme +
	color_dictionary +
	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
ggsave("report/plots/lambda.png", width = 25, height = 15, units = "cm")


filter(data_lambda, !is.na(avg_profits)) %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	# filter(Delta > 0) %>%
	ggplot(aes(x = fct_rev(varied_parameter_fct), y = Delta, fill = feature_method)) +
	geom_boxplot(position = "dodge") +
	geom_hline(yintercept = c(0, 1)) +
	scale_y_continuous(expand = c(0, 0), breaks = seq(0,1, by = 0.25), labels = c(expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	# theme_tq() +
	# scale_x_continuous(minor_breaks = variations, breaks = variations) +
	labs(x = expression(lambda), y = expression(Delta)) +
	ai_theme


# Learning Phase Trajectory  ----------------------------------------------------------

experiment_ids <- 7; variations[experiment_ids]
# 
# 
# 
outcomes <- data_lambda %>%
	select(-intervention, -avg_profits) %>%
	unnest(outcomes) %>%
	filter(!is.na(t_group))

t_grouping <- 50000

# manually_optimized_alpha <- data %>%
# 	filter(
# 		(feature_method == "poly-separated" & varied_parameter == "1e-06") |
# 			(feature_method == "poly-tiling" & varied_parameter == "1e-08") |
# 			(feature_method == "tiling" & varied_parameter == "0.001")  |
# 			(feature_method == "tabular" & varied_parameter == "0.1")
# 	)


# outcomes <- manually_optimized_alpha %>%
# 	select(-intervention, -avg_profits) %>%
# 	unnest(outcomes)




learning_phase <- outcomes %>%
	# complete(feature_method, varied_parameter, run_id, t_group) %>%
	# group_by(feature_method, varied_parameter, run_id) %>%
	# fill(price, Delta, .direction = "down") %>%
	pivot_longer(cols = c(price, Delta), names_to = "metric") %>%
   filter(varied_parameter %in% variations[experiment_ids])

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
# ggsave("report/plots/all_runs.png", width = 25, height = 20, units = "cm")



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
	theme_tq() +
	color_dictionary


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
# ggsave("report/plots/average_intervention.png", width = 25, height = 20, units = "cm")

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
# ggsave("report/plots/intervention_violin.png", width = 25, height = 25, units = "cm")