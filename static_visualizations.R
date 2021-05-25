library(tidyverse)
library(tidyquant)
library(rlang)
library(ggbeeswarm)
library(xtable)

source("economic_environment.R")
source("visualization_helper.R")



# Convergence --------------------------------------------------------
load("simulation_results/Alpha_final/aggregated.RData")

# invoke function from above to create stacked plot with convergence info
convergence_info <- convergence_plot(data = data,
												 varied = "varied_parameter",
												 filter_cond = "!(feature_method == 'tabular' & varied_parameter %in% c('1e-06', '1e-07', '1e-08', '1e-10', '1e-12')) &
					  !(feature_method == 'tiling' & varied_parameter == '1e-12')",
												 convergence_max = 500000,
												 runs_per_experiment = 48,
												 x_lab = expression(alpha))
ggsave("report/plots/converged.png", width = 25, height = 15, units = "cm")

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
	fill_dictionary +
	guides(col = FALSE)
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
	ai_theme +
	labs(y = "") +
	fill_dictionary +
	guides(col = FALSE)
ggsave("report/plots/trajectory_Delta.png", width = 25, height = 20, units = "cm")

learning_phase %>%
	filter(metric == "price") %>%
	mutate(t = as_factor(format(t_group * t_grouping, scientific = FALSE))) %>%
	ggplot(aes(x = t, y = value, fill = feature_method)) +
	geom_hline(yintercept = c(1.472927,1.92498), linetype = "dashed") +	
	geom_violin(position = "dodge", scale = "count", draw_quantiles = 0.5) +
	facet_grid(metric~feature_method, scales = "free_y") +
	facet_wrap(~feature_method, nrow = 4, scales = "free_y") +
	ai_theme +
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
	guides(colour = FALSE)
ggsave("report/plots/price_range.png", width = 25, height = 15, units = "cm")



# Deviation ------------------------------------------------------------


intervention <- deviation_plot(data = manually_optimized_alpha, varied = "feature_method")
ggsave("report/plots/average_intervention.png", width = 25, height = 20, units = "cm")


counterfactual <- counterfactual_plot(data = intervention, varied = "feature_method")
ggsave("report/plots/intervention_boxplot.png", width = 25, height = 25, units = "cm")


counterfactual_plot(data = intervention, varied = "feature_method", y_axis_variable = "profit")
ggsave("report/plots/intervention_profit_boxplot.png", width = 25, height = 25, units = "cm")



deviation_profitability <- counterfactual %>%
	filter(metric == "profit", tau > 0) %>%
	mutate(actual_discounted = actual * 0.95^(tau - 1),
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%
	group_by(feature_method, run_id, agent) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%
	mutate(diff_discounted = actual_discounted - counterfactual_discounted)

deviation_profitability_table <- deviation_profitability %>%
	group_by(feature_method, agent) %>%
	summarize("share profitable" = mean(diff_discounted > 0),
				 "share unprofitable" = mean(diff_discounted < 0)); deviation_profitability_table

print(xtable(deviation_profitability_table, type = "latex"),
		file = "report/tables/share_deviation_profitability.tex",
		floating = FALSE,
		include.rownames = FALSE)


ggplot(deviation_profitability, aes(x = diff_discounted, col = feature_method)) +
	geom_freqpoly(size = 1, binwidth = 0.02, alpha = 0.7) +
	facet_wrap(~agent, ncol = 1) +
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
	color_dictionary +
	guides(col = FALSE)








# Vary Beta ---------------------------------------------------------------
load("simulation_results/Beta_final/aggregated.RData")

data_beta <- manually_optimized_alpha %>%
	mutate(varied_parameter = "4e-05",
			 varied_parameter_fct = as.factor("4e-05")) %>%
	bind_rows(data)

# specify maximum run length for particular values of beta
beta_TT <- c("0.00016"  = 125000,
				 "8e-05"    = 250000,
				 "4e-05"    = 500000,
				 "2e-05"    = 1000000,
				 "1e-05"    = 2000000) %>%
	enframe(name = "varied_parameter", value = "convergence_threshold")


# Bar chart of convergence proportions
convergence_info_beta <- convergence_plot(data = data_beta,
													varied = "varied_parameter",
													convergence_max = beta_TT,
													runs_per_experiment = 48,
													x_lab = expression(beta))
ggsave("report/plots/converged_beta.png", width = 25, height = 15, units = "cm")


# average Delta
beta_delta <- point_line_plot(data = data_beta, varied = "varied_parameter", x_lab = expression(beta), x_log10 = TRUE)
ggsave("report/plots/beta.png", width = 25, height = 15, units = "cm")


# average deviation trajectory for tabular learning
intervention_beta_tabular <- deviation_plot(data = data_beta, varied = "varied_parameter_fct",
												filter_cond = "feature_method == 'tabular' & varied_parameter != '4e-05'",
												no_col = TRUE)
ggsave("report/plots/average_intervention_beta_tabular.png", width = 25, height = 15, units = "cm")

# price comparison with counterfactual (adhering to learned strategies)
counterfactual_plot(data = intervention_beta_tabular, varied = "varied_parameter_fct", no_col = TRUE)

# average deviation trajectory for beta = 0.00016 & beta = 1e-05
deviation_plot(data = data_beta, varied = "feature_method", filter_cond = "varied_parameter == '0.00016'")  # no real punishment throughout methods
deviation_plot(data = data_beta, varied = "feature_method", filter_cond = "varied_parameter == '1e-05'")    # clear punishment for tabular learning, not for other methods

# share of profitable deviations as a function of beta
intervention_beta <- deviation_plot(data = data_beta, noplot = TRUE)
counterfactual_beta <- counterfactual_plot(data = intervention_beta, varied = c("varied_parameter_fct", "feature_method"), noplot = TRUE)






deviation_profitability_beta <- counterfactual_beta %>%
	filter(metric == "profit", tau > 0) %>%
	mutate(actual_discounted = actual * 0.95^(tau - 1),
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%
	group_by(feature_method, varied_parameter_fct, run_id, agent) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%
	mutate(diff_discounted = actual_discounted - counterfactual_discounted) 

deviation_profitability_table_beta <- deviation_profitability_beta %>%
	group_by(feature_method, varied_parameter_fct, agent) %>%
	summarize("share profitable" = mean(diff_discounted > 0)) %>%
	pivot_wider(names_from = "varied_parameter_fct", names_prefix = "$\\beta =$ ", values_from = 'share profitable') %>%
	select(feature_method, agent, "$\\beta =$ 0.00016", "$\\beta =$ 8e-05", "$\\beta =$ 4e-05", "$\\beta =$ 2e-05", "$\\beta =$ 1e-05") %>%
	rename("feature method" = "feature_method"); deviation_profitability_table_beta


print(xtable(deviation_profitability_table_beta, type = "latex"),
		file = "report/tables/share_deviation_profitability_beta.tex",
		floating = FALSE,
		sanitize.colnames.function = identity,
		include.rownames = FALSE)






# Vary Lambda -------------------------------------------------------------

load("simulation_results/Lambda_final/aggregated.RData")

# (row-) bind benchmark with optimal alphas with new data
data_lambda <- manually_optimized_alpha %>%
	mutate(varied_parameter = "0.5",
			 varied_parameter_fct = as.factor("0.5")) %>%
	bind_rows(data)

convergence_info_lambda <- convergence_plot(data = data_lambda,
														varied = "varied_parameter",
														convergence_max = 500000,
														runs_per_experiment = 48,
														x_lab = expression(lambda))
ggsave("report/plots/converged_lambda.png", width = 25, height = 15, units = "cm")


# average Delta
lambda_delta <- point_line_plot(data = data_lambda, varied = "varied_parameter", x_lab = expression(lambda), x_log10 = FALSE)
ggsave("report/plots/lambda.png", width = 25, height = 15, units = "cm")


data_lambda %>%
	filter(varied_parameter != "0.5") %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	ggplot(aes(x = fct_rev(varied_parameter_fct), y = Delta, fill = feature_method)) +
	geom_hline(yintercept = c(0, 1), linetype = "dashed") +
	scale_y_continuous(expand = c(0, 0), breaks = seq(-0.5,1, by = 0.25), labels = c("-0.5", "-0,25", expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	geom_violin(draw_quantiles = 0.5, color = "grey35", scale = "width") +
	labs(x = expression(alpha), y = expression(Delta)) +
	ai_theme +
	facet_wrap(~feature_method) +
	fill_dictionary +
	guides(col = FALSE)
ggsave("report/plots/lambda_violin.png", width = 25, height = 15, units = "cm")


# sub-nash outcomes are mostly a result of non-converged runs
data_lambda %>%
	filter(feature_method == "poly-separated") %>%
	select(-intervention, -avg_profits) %>%
	unnest(outcomes) %>%
	pivot_longer(cols = c(price, Delta), names_to = "metric") %>%
	mutate(p_n = ifelse(metric == "Delta", 0, 1.47),
			 p_m = ifelse(metric == "Delta", 1, 1.93 )) %>%
	ggplot(aes(x = t_group * t_grouping, y = value, group = interaction(feature_method, run_id, varied_parameter), col = feature_method)) +
	geom_hline(aes(yintercept = p_n), linetype = "dashed") +	
	geom_hline(aes(yintercept = p_m), linetype = "dashed") +	
	geom_line(alpha = 0.3) +
	facet_wrap(~metric, ncol =1, scales = "free_y") +
	ai_theme +
	scale_x_continuous(labels = scales::comma) +
	color_dictionary +
	labs(x = "t", y = " ") +
	guides(colour = guide_legend(override.aes = list(alpha = 1)))


# deviation patterns are largely independent of lambda

# average deviation trajectory for lambda = 0 & lambda = 0.9
deviation_plot(data = data_lambda, varied = "feature_method", filter_cond = "varied_parameter == '0'")      # clear deviation pattern only with tabular learning 
deviation_plot(data = data_lambda, varied = "feature_method", filter_cond = "varied_parameter == '0.9'")    # clear deviation pattern only with tabular learning 


intervention_lambda_tabular <- deviation_plot(data = data_lambda, varied = "varied_parameter_fct",
														  filter_cond = "feature_method == 'tabular' & varied_parameter != '0.5'",
														  no_col = TRUE)

# price comparison with counterfactual (adhering to learned strategies)
counterfactual_plot(data = intervention_lambda_tabular, varied = "varied_parameter_fct", no_col = TRUE)




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

# average Delta
m_delta <- point_line_plot(data = data_m, varied = "varied_parameter", x_lab = "m", x_log10 = FALSE)
ggsave("report/plots/m.png", width = 25, height = 15, units = "cm")

# average deviation trajectory for m = 10 and  m = 63 
intervention_m_10 <- deviation_plot(data = data_m, varied = "varied_parameter", filter_cond = "varied_parameter == '10'")
ggsave("report/plots/average_intervention_m_10.png", width = 25, height = 20, units = "cm")

deviation_plot(data = data_m, varied = "varied_parameter", filter_cond = "varied_parameter == '63'")

# price comparison with counterfactual (adhering to learned strategies)
counterfactual_m_10 <- counterfactual_plot(data = intervention_m_10, varied = "varied_parameter")
ggsave("report/plots/intervention_boxplot_m_10.png", width = 25, height = 25, units = "cm")


# share of profitable deviations as a function of m
intervention_m <- deviation_plot(data = data_m, varied = "varied_parameter", noplot = TRUE)
counterfactual_m <- counterfactual_plot(data = intervention_m, varied = "varied_parameter")





deviation_profitability_m <- counterfactual_m %>%
	mutate(player = ifelse(player == 1, "deviating agent", "non deviating agent")) %>%
	filter(metric == "profit", tau > 0) %>%
	mutate(actual_discounted = actual * 0.95^(tau - 1),
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%
	group_by(feature_method, varied_parameter, run_id, player) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%
	mutate(diff_discounted = actual_discounted - counterfactual_discounted) 

deviation_profitability_table_m <- deviation_profitability_m %>%
	group_by(feature_method, varied_parameter, player) %>%
	summarize("share profitable" = mean(diff_discounted > 0)) %>%
	pivot_wider(names_from = "varied_parameter", names_prefix = "m = ", values_from = 'share profitable'); deviation_profitability_table_m


print(xtable(deviation_profitability_table_m, type = "latex"),
		file = "report/tables/share_deviation_profitability_m.tex",
		floating = FALSE,
		include.rownames = FALSE)





# Vary gamma ------------------------------------------------------------------

load("simulation_results/Gamma_final/aggregated.RData")

data_gamma <- manually_optimized_alpha %>%
	mutate(varied_parameter = "0.95",
			 varied_parameter_fct = as.factor("0.95")) %>%
	bind_rows(data)

# Bar chart of convergence proportions
convergence_info_gamma <- convergence_plot(data = data_gamma,
														varied = "varied_parameter",
														convergence_max = 500000,
														runs_per_experiment = 48,
														x_lab = expression(gamma))
ggsave("report/plots/converged_gamma.png", width = 25, height = 15, units = "cm")


# average Delta
gamma_delta <- point_line_plot(data = data_gamma, varied = "varied_parameter", x_lab = expression(gamma), x_log10 = FALSE)
ggsave("report/plots/gamma.png", width = 25, height = 15, units = "cm")

# distribution of average prices 
data_gamma %>%
	filter(!is.na(avg_prices)) %>%
	ggplot(aes(x = fct_rev(varied_parameter_fct), y = avg_prices, fill = feature_method)) +
	geom_hline(yintercept = c(1.47, 1.92), linetype = "dashed") +
	geom_violin(draw_quantiles = 0.5, color = "grey35", scale = "width") +
	labs(x = expression(gamma), y = "average price") +
	ai_theme +
 	coord_cartesian(ylim = c(1.3, 2)) +
	facet_wrap(~feature_method) +
	fill_dictionary +
	guides(col = FALSE)
ggsave("report/plots/gamma_violin_price.png", width = 25, height = 15, units = "cm")


# average deviation trajectory for tabular learning
intervention_gamma_tabular <- deviation_plot(data = data_gamma, varied = "varied_parameter_fct",
														  filter_cond = "feature_method == 'tabular' & varied_parameter != '0.95'",
														  no_col = TRUE)
ggsave("report/plots/average_intervention_gamma_tabular.png", width = 25, height = 15, units = "cm")

# price comparison with counterfactual (adhering to learned strategies)
counterfactual_plot(data = intervention_gamma_tabular, varied = "varied_parameter_fct", no_col = TRUE)

# average deviation trajectory for gamma = 0, 0.9 & 1
deviation_plot(data = data_gamma, varied = "feature_method", filter_cond = "varied_parameter == '0'")  # no punishment throughout methods
deviation_plot(data = data_gamma, varied = "feature_method", filter_cond = "varied_parameter == '0.9'")    # clearest punishment for tabular learning, not for other methods
deviation_plot(data = data_gamma, varied = "feature_method", filter_cond = "varied_parameter == '1'")    # surprsinginly consistent with previous results




# share of profitable deviations as a function of gamma
intervention_gamma <- deviation_plot(data = data_gamma, noplot = TRUE)
counterfactual_gamma <- counterfactual_plot(data = intervention_gamma, varied = c("varied_parameter_fct", "feature_method"), noplot = TRUE)

deviation_profitability_gamma <- counterfactual_gamma %>%
	filter(metric == "profit", tau > 0) %>%
	mutate(actual_discounted = actual * 0.95^(tau - 1),
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%
	group_by(feature_method, varied_parameter_fct, run_id, agent) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%
	mutate(diff_discounted = actual_discounted - counterfactual_discounted) 

deviation_profitability_table_gamma <- deviation_profitability_gamma %>%
	group_by(feature_method, varied_parameter_fct, agent) %>%
	summarize("share profitable" = mean(diff_discounted > 0)) %>%
	pivot_wider(names_from = "varied_parameter_fct", names_prefix = "$\\gamma =$ ", values_from = 'share profitable') %>%
	select(feature_method, agent, "$\\beta =$ 0.00016", "$\\beta =$ 8e-05", "$\\beta =$ 4e-05", "$\\beta =$ 2e-05", "$\\beta =$ 1e-05") %>%
	rename("feature method" = "feature_method"); deviation_profitability_table_beta