library(tidyverse)
library(rlang)
library(ggbeeswarm)
library(xtable)

source("economic_environment.R")
source("visualization_helper.R")



# 6.1 - Convergence  --------------------------------------------------------

load("simulation_results/Alpha_final/aggregated.RData")           #l oad file with aggregated results

# stackec plot with convergence info by FEM, filter condition ensures experiments that were not attempted do not appear as bars
convergence_info <- convergence_plot(data = data,
												 varied = "varied_parameter",
												 filter_cond = "!(FEM == 'Tabular' & varied_parameter %in% c('1e-06', '1e-07', '1e-08', '1e-10', '1e-12')) &
					  							!(FEM %in% c('Polynomial Tiles', 'Separate Polynomials') & varied_parameter %in% c(0.1, 0.01))",
												 convergence_max = 500000,
												 runs_per_experiment = 48,
												 x_lab = expression(alpha))
ggsave("report/plots/converged.png", width = 25, height = 15, units = "cm")

# Out of non-failed runs, get share of converged runs
convergence_info %>%
	count(status, wt = value) %>%
	filter(status != "failed") %>%
	mutate(prop = n/sum(n))

# Out of non-failed runs, get share of converged runs by FEM
convergence_info %>%
	group_by(FEM) %>%
	count(status, wt = value) %>%
	filter(status != "failed") %>%
	mutate(prop = n/sum(n))


# frequency polygon showing time of convergence by FEM
data %>%
	filter(!is.na(cycle_length)) %>%   #ONLY INCLUDE CONVERGED RUNS
	ggplot(aes(x = convergence, y = stat(density), col = FEM)) +
	geom_freqpoly(size = 1.5, binwidth = 8000, alpha = 0.7) +
	scale_x_continuous(limits = c(0, 500000), labels = scales::comma) +
	color_dictionary +
	labs(x = "t") +
	ai_theme +
	guides(colour = guide_legend(override.aes = list(alpha = 1)))
ggsave("report/plots/convergence_at.png", width = 25, height = 12, units = "cm")	


# Price Cycles

# Count runs by cycle length
data %>%
	ggplot(aes(x = as.factor(cycle_length), fill = FEM)) +
	geom_bar(position = "stack", na.rm = TRUE) +
	scale_x_discrete(na.translate = FALSE) +
	ai_theme +
	fill_dictionary +
	labs(x = "cycle length", y = "")
ggsave("report/plots/cycle_length.png", width = 25, height = 15, units = "cm")


# retrieve count for specific FEM-cycle length combinations
data %>%
	group_by(FEM) %>%
	count(cycle_length) %>%
	filter(
		(FEM == "Tabular" & cycle_length == 10) |
		(FEM == "Tile Coding" & cycle_length == 2)
	)


# 6.2 - Profits --------------------------------------------------------------

# invoke general function to get plot of Delta by FEM and alpha, filter condition makes sure negative data points are excluded (for better visibility)
alpha_delta <- point_line_plot(data = data, varied = "varied_parameter", filter_cond = "Delta >= 0", x_lab = expression(alpha),
										 x_log10 = TRUE, with_authors = TRUE)
ggsave("report/plots/alpha.png", width = 25, height = 15, units = "cm")

# maximaum alpha by FEM
alpha_delta %>%
	group_by(FEM) %>%
	summarize(max = max(Delta))

# same as above, this time ONLY negative data points, precise values are printed out
sub_nash_data <- point_line_plot(data = data, varied = "varied_parameter", filter_cond = "Delta < 0", x_lab = expression(alpha), x_log10 = TRUE)
sub_nash_data


# derive Delta from profits
all_runs <- data %>%
	mutate(Delta = get_delta(avg_profits))


# filter out 'unreasonable experiments' and single outlier run for better presentabiliity
all_runs_plot <- all_runs %>%
	anti_join(sub_nash_data, by = c("FEM", "varied_parameter")) %>%  # filters out experiments with average negative Delta
	filter(Delta >= -0.5)  #filters out single outlier
	
	

all_runs_plot %>%	
	ggplot(aes(x = fct_rev(varied_parameter_fct), y = Delta, fill = FEM)) +
	geom_hline(yintercept = c(0, 1), linetype = "dashed") +
	scale_y_continuous(expand = c(0, 0), breaks = seq(-0.5,1, by = 0.25), labels = c("-0.5", "-0,25", expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	geom_violin(draw_quantiles = 0.5, color = "grey35", scale = "width") +
	labs(x = expression(alpha), y = expression(Delta)) +
	ai_theme +
	facet_wrap(~FEM) +
	fill_dictionary +
	guides(fill = FALSE) +
	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
ggsave("report/plots/alpha_violin.png", width = 25, height = 15, units = "cm")


# zoom in on separate polynomials and get percentage of sub-nash runs
poly_sep <- filter(all_runs, FEM == "Separate Polynomials")

# calculate percentage of runs converging in sub-nash profits
poly_sep %>%
	count(sub_nash = (Delta <= 0)) %>%
	mutate(perc = n / sum(n))

# calculate percentage of runs converging in sub-nash profits by FEM
poly_sep %>%
	group_by(varied_parameter) %>%
	count(sub_nash = (Delta <= 0)) %>%
	mutate(perc = n / sum(n)) %>%
	arrange(sub_nash, perc)


# extract experiment with 'optimal' alphas for upcoming analyses
manually_optimized_alpha <- data %>%
	filter(
		(FEM == "Separate Polynomials" & varied_parameter == "1e-06") |
			(FEM == "Polynomial Tiles" & varied_parameter == "1e-08") |
			(FEM == "Tile Coding" & varied_parameter == "0.001")  |
			(FEM == "Tabular" & varied_parameter == "0.1")
	)


# Appendix A2 - Price Trajectory  ----------------------------------------------------------

# define number of aggregated periods (this is to adjust values of t on x-axis, the actual aggregation of periods happens in  'aggregate_files.R')
t_grouping <- 50000

# unnest aggregated time frames
outcomes <- manually_optimized_alpha %>%
	select(-intervention, -avg_profits) %>%
	unnest(outcomes)

# wrangle metrics into long format
learning_phase <- outcomes %>%
	pivot_longer(cols = c(price, Delta), names_to = "metric")


# print trajextory of every single run
learning_phase %>%
	mutate(p_n = ifelse(metric == "Delta", 0, 1.472927),          # prepare reference lines
			 p_m = ifelse(metric == "Delta", 1, 1.92498 )) %>%
	ggplot(aes(x = t_group * t_grouping, y = value, group = interaction(FEM, run_id, varied_parameter), col = FEM)) +
	geom_hline(aes(yintercept = p_n), linetype = "dashed") +	
	geom_hline(aes(yintercept = p_m), linetype = "dashed") +	
	geom_line(alpha = 0.4) +
	facet_wrap(~metric, ncol =1, scales = "free_y") +
	ai_theme +
	scale_x_continuous(labels = scales::comma) +
	color_dictionary +
	labs(x = "t", y = " ") +
	guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1.3))) +
	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
ggsave("report/plots/all_runs.png", width = 25, height = 15, units = "cm")


#  violin plot of distribution of Delta over time
learning_phase %>%
	filter(metric == "Delta") %>%   # only Delta
	mutate(t = as_factor(format(t_group * t_grouping, scientific = FALSE))) %>%    # time variable
	ggplot(aes(x = t, y = value, fill = FEM)) +
	geom_hline(yintercept = c(0,1), linetype = "dashed") +	                     # reference lines
	geom_violin(position = "dodge", scale = "count", draw_quantiles = 0.5) +
	facet_wrap(~FEM, nrow = 4) +
	ai_theme +
	labs(x = "t", y = expression(Delta)) +
	fill_dictionary +
	guides(fill = FALSE) +
	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
ggsave("report/plots/trajectory_Delta.png", width = 25, height = 17, units = "cm")

#  violin plot of distribution of prices over time
learning_phase %>%
	filter(metric == "price") %>%   # only price
	mutate(t = as_factor(format(t_group * t_grouping, scientific = FALSE))) %>%  # time variable
	ggplot(aes(x = t, y = value, fill = FEM)) +
	geom_hline(yintercept = c(1.472927, 1.92498), linetype = "dashed") +	      # reference lines
	geom_violin(position = "dodge", scale = "count", draw_quantiles = 0.5) +
	facet_wrap(~FEM, nrow = 4) +
	ai_theme +
	labs(y = "average price") +
	fill_dictionary +
	guides(fill = FALSE)
ggsave("report/plots/trajectory_price.png", width = 25, height = 17, units = "cm")



# Appendix A.1 Price Range -------------------------------------------------------------

# price range within a cycle
price_range <- manually_optimized_alpha %>%
	filter(!is.na(cycle_length)) %>%   #  only converged runs
	select(FEM, varied_parameter, run_id, intervention, cycle_length) %>%
	unnest(intervention) %>%   # unnest intervention data
	filter(tau <= 0) %>%   #zoom in on 'pre-intervention' episodes
	pivot_longer(cols = c(price_1, price_2), names_to = "price") %>%
	group_by(FEM, run_id, price) %>%
	summarize(min = min(value),
				 max = max(value),
				 range = max - min,
				 cycle_length = unique(cycle_length)) %>%
	group_by(FEM, cycle_length) %>%
	mutate(n = n())


# price range within a cycle, every point is one agent (potential warning messages due to version conflict, can be ignored)
ggplot(price_range, aes(x = as.factor(cycle_length), y = range, col = FEM)) +
	geom_hline(yintercept = (1.92498 - 1.472927)) +     # reference line: difference between nash and fully collusive prices
	geom_quasirandom(size = 1.3, width = 0.6, varwidth = TRUE, dodge.width = 0.2) +
	facet_wrap(~FEM, scales = "free_x") +
	ai_theme +
	fill_dictionary +
	labs(x = "cycle length", y = "price range") +
	guides(colour = FALSE)
ggsave("report/plots/price_range.png", width = 25, height = 15, units = "cm")



# 6.3 Deviation ------------------------------------------------------------

# plot price trajectory around intervention
intervention <- deviation_plot(data = manually_optimized_alpha, varied = "FEM")
ggsave("report/plots/average_intervention.png", width = 25, height = 15, units = "cm")

# plot prices development relative to counterfactual path had no deviation taken place
counterfactual <- counterfactual_plot(data = intervention, varied = "FEM")
ggsave("report/plots/intervention_boxplot.png", width = 25, height = 22, units = "cm")

# plot profits development relative to counterfactual path had no deviation taken place
counterfactual_plot(data = intervention, varied = "FEM", y_axis_variable = "profit")
ggsave("report/plots/intervention_profit_boxplot.png", width = 25, height = 22, units = "cm")


# calculate whether deviation was profitable
deviation_profitability <- counterfactual %>%
	filter(metric == "profit", tau > 0) %>%   # filter profits
	mutate(actual_discounted = actual * 0.95^(tau - 1),    # discounted profits in deviation episode
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%  # discountedprofits in counterfactual (without deviation)
	group_by(FEM, run_id, agent) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%  # sum over disocunted profits to get net present value
	mutate(diff_discounted = actual_discounted - counterfactual_discounted)  # calculate difference in NPV

# get share of profitable deviations 
deviation_profitability_table <- deviation_profitability %>%
	mutate(agent = str_sub(agent, end = -7L)) %>%
	group_by(FEM, agent) %>%
	summarize("share profitable" = mean(diff_discounted > 0), 
				 "share unprofitable" = mean(diff_discounted < 0)); deviation_profitability_table
	
# save as Latex table
print(xtable(deviation_profitability_table, type = "latex"),
		file = "report/tables/share_deviation_profitability.tex",
		floating = FALSE,
		include.rownames = FALSE)





# Appendix 6.3 - Deviation ------------------------------------------------


# frequency polygon of deviation profitability by FEM
deviation_profitability %>%
	ggplot(aes(x = diff_discounted, col = FEM)) +
		geom_freqpoly(size = 1, binwidth = 0.02, alpha = 0.7) +
		facet_wrap(~agent, ncol = 1) +
		scale_x_continuous(limits = c(-0.5, 0.5)) +
		ai_theme +
		color_dictionary +
		guides(colour = guide_legend(override.aes = list(alpha = 1))) +
		labs(x = expression(pi), y = "")
ggsave("report/plots/intervention_profitabiliy_polygon.png", width = 25, height = 15, units = "cm")


# not displayed in plot above
filter(deviation_profitability, (diff_discounted <= -0.5 |  diff_discounted >= 0.5))


# 3 exemplary runs to exhibit behavior of polynomial tiles non deviating agent
intervention %>%
	filter(FEM == "Polynomial Tiles", tau > -3, run_id %in% c(6, 8, 9)) %>%
	pivot_longer(c("price_1", "price_2", "profit_1", "profit_2"), names_to = "temp") %>%
	separate(temp, into = c("metric", "player"), sep = "_") %>%
	mutate(p_n = ifelse(metric == "profit", 0.223, 1.472927),
			 p_m = ifelse(metric == "profit", 0.337, 1.92498),
			 run_id = str_pad(run_id, 2, pad = "0"),
			 player = ifelse(player == 1, "deviating agent", "non deviating agent")) %>%
	ggplot(aes(x = tau, y = value, shape = player, linetype = player)) +
	geom_hline(aes(yintercept = p_n), linetype = "dashed") +	
	geom_hline(aes(yintercept = p_m), linetype = "dashed") +
	geom_vline(xintercept = 0, linetype = "dotted") +
	geom_line() +
	geom_point() +
	facet_grid(metric~run_id, scales = "free_y") +
	scale_x_continuous(labels = scales::comma) +
	ai_theme +
	labs(x  = expression(tau), y = "")
ggsave("report/plots/intervention_poly_tiling.png", width = 25, height = 15, units = "cm")



	
# 7.1 - Prolonged Intervention --------------------------------------------

# load prolonged intervention data
load("simulation_results/prolonged_deviation/aggregated.RData")

# unnest deviation data and filter on deviation length = 10
prolonged <- data %>%
	select(FEM, varied_parameter, run_id, intervention_prolonged, cycle_length) %>%
	unnest(intervention_prolonged) %>%
	filter(intervention_length == 10)

# deviation price trajectory by FEM
prolonged %>%
	group_by(FEM, tau) %>%
	summarize("deviating" = mean(price_1),
				 "non deviating" = mean(price_2)) %>%
	pivot_longer(cols = c("deviating", "non deviating"), names_to = "price") %>%
	filter(tau > -2) %>%
	ggplot(aes(x = tau, y = value, linetype = price, shape = price, col = FEM)) +
	geom_hline(yintercept = c(1.472927, 1.92498), linetype = "dashed") +
	geom_vline(xintercept = 0, linetype = "dotted") +
	geom_point(size = 3) +
	geom_line(size = 1) +
	scale_linetype_manual(values = c("solid", "dotted")) +
	facet_wrap(~FEM, nrow = 2) +
	labs(x = expression(tau), y = "average prices") +
	ai_theme +
	color_dictionary +
	guides(col = FALSE)
ggsave("report/plots/average_prolonged_intervention.png", width = 25, height = 15, units = "cm")

# determine counterfactual path had no deviation taken place
prolonged_counterfactual <- prolonged  %>%
	filter(!is.na(cycle_length)) %>%    # remove not-converged runs because the counterfactual is not clear
	pivot_longer(cols = c("price_1", "price_2", "profit_1", "profit_2"), names_to = "obs", values_to = "actual") %>%
	group_by(FEM, run_id, obs) %>%
	mutate(replacement_pos = ifelse(tau <=0, NA, row_number() - cycle_length * (1 + (row_number() - 11) %/% cycle_length))) %>%
	mutate(counterfactual = ifelse(is.na(replacement_pos), actual, actual[replacement_pos]),
			 diff = actual - counterfactual) %>%
	separate(col = obs, into = c("metric", "agent"), sep = "_") %>%
	mutate(agent = ifelse(agent == "1", "deviating agent", "non deviating agent"))


# Appendix B1 - Prolonged Deviation ---------------------------------------

# print range of prices relative to counterfactual
prolonged_counterfactual %>%
	filter(tau >= 0, metric == "price") %>%
	ggplot(aes(x = as.factor(tau), y = diff, fill = FEM)) +
	stat_summary(fun.data = full_whisker, geom = "errorbar", width = 0.5) +
	stat_summary(fun.data = full_whisker, geom = "boxplot", col = "gray35") +
	facet_grid(FEM~agent) +
	ai_theme +
	fill_dictionary +
	labs(x = expression(tau), y = "price difference") +
	guides(fill = FALSE)
ggsave("report/plots/prolonged_intervention_boxplot.png", width = 25, height = 22, units = "cm")



# 7.2 - Learning Parameters & Appendix B.2 ---------------------------------------------------------------

# load beta experiments
load("simulation_results/Beta_final/aggregated.RData")

# concatenate with existing default values from optimized experiment
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


# average Delta
beta_delta <- point_line_plot(data = data_beta, varied = "varied_parameter", x_lab = expression(beta), x_log10 = TRUE)
ggsave("report/plots/beta.png", width = 25, height = 15, units = "cm")


# average deviation trajectory for tabular learning
intervention_beta_tabular <- deviation_plot(data = data_beta, varied = "varied_parameter_fct",
												filter_cond = "FEM == 'Tabular' & varied_parameter != '4e-05'",  # filters for tabular learning experiments
												no_col = TRUE)
ggsave("report/plots/average_intervention_beta_tabular.png", width = 25, height = 15, units = "cm")

# price comparison with counterfactual (adhering to learned strategies)
counterfactual_plot(data = intervention_beta_tabular, varied = "varied_parameter_fct", no_col = TRUE)

# average deviation trajectory for beta = 0.00016 & beta = 1e-05
deviation_plot(data = data_beta, varied = "FEM", filter_cond = "varied_parameter == '0.00016'")  # no real punishment throughout methods
deviation_plot(data = data_beta, varied = "FEM", filter_cond = "varied_parameter == '1e-05'")    # clear punishment for tabular learning, not for other methods

# share of profitable deviations as a function of beta
intervention_beta <- deviation_plot(data = data_beta, noplot = TRUE)
counterfactual_beta <- counterfactual_plot(data = intervention_beta, varied = c("varied_parameter_fct", "FEM"), noplot = TRUE)


deviation_profitability_beta <- counterfactual_beta %>%
	filter(metric == "profit", tau > 0) %>%
	mutate(actual_discounted = actual * 0.95^(tau - 1),
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%
	group_by(FEM, varied_parameter_fct, run_id, agent) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%
	mutate(diff_discounted = actual_discounted - counterfactual_discounted) 

deviation_profitability_table_beta <- deviation_profitability_beta %>%
	mutate(agent = str_sub(agent, end = -7L)) %>%
	group_by(FEM, varied_parameter_fct, agent) %>%
	summarize("share profitable" = mean(diff_discounted > 0)) %>%
	pivot_wider(names_from = "varied_parameter_fct", names_prefix = "$\\beta =$ ", values_from = 'share profitable') %>%
	select(FEM, agent, "$\\beta =$ 0.00016", "$\\beta =$ 8e-05", "$\\beta =$ 2e-05", "$\\beta =$ 1e-05") 


print(xtable(deviation_profitability_table_beta, type = "latex"),
		file = "report/tables/share_deviation_profitability_beta.tex",
		floating = FALSE,
		sanitize.colnames.function = identity,
		include.rownames = FALSE)






# Appendix B.3 - Lambda-------------------------------------------------------------

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


data_lambda %>%
	filter(varied_parameter != "0.5") %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	ggplot(aes(x = fct_rev(varied_parameter_fct), y = Delta, fill = FEM)) +
	geom_hline(yintercept = c(0, 1), linetype = "dashed") +
	scale_y_continuous(expand = c(0, 0), breaks = seq(-0.5,1, by = 0.25), labels = c("-0.5", "-0,25", expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	geom_violin(draw_quantiles = 0.5, color = "grey35", scale = "width") +
	labs(x = expression(lambda), y = expression(Delta)) +
	ai_theme +
	facet_wrap(~FEM) +
	fill_dictionary +
	guides(fill = FALSE) +
	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
ggsave("report/plots/lambda_violin.png", width = 25, height = 15, units = "cm")


# sub-nash outcomes are mostly a result of non-converged runs
data_lambda %>%
	filter(FEM == "Separate Polynomials") %>%
	select(-intervention, -avg_profits) %>%
	unnest(outcomes) %>%
	pivot_longer(cols = c(price, Delta), names_to = "metric") %>%
	mutate(p_n = ifelse(metric == "Delta", 0, 1.472927),
			 p_m = ifelse(metric == "Delta", 1, 1.92498)) %>%
	ggplot(aes(x = t_group * t_grouping, y = value, group = interaction(FEM, run_id, varied_parameter), col = FEM)) +
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
deviation_plot(data = data_lambda, varied = "FEM", filter_cond = "varied_parameter == '0'")      # clear deviation pattern only with tabular learning 
deviation_plot(data = data_lambda, varied = "FEM", filter_cond = "varied_parameter == '0.9'")    # clear deviation pattern only with tabular learning 

# tabular learning evokes a punishment for very much all values of lambda
intervention_lambda_tabular <- deviation_plot(data = data_lambda, varied = "varied_parameter_fct",
														  filter_cond = "FEM == 'Tabular' & varied_parameter != '0.5'",
														  no_col = TRUE)

# price comparison with counterfactual (adhering to learned strategies)
counterfactual_plot(data = intervention_lambda_tabular, varied = "varied_parameter_fct", no_col = TRUE)




# 7.3 Price Grid $ Appendix B.4 ------------------------------------------------------------------


# load experiments with alternative m (number of available prices)
load("simulation_results/m_final/aggregated.RData")

data_m <- manually_optimized_alpha %>%
	mutate(varied_parameter = "19",
			 varied_parameter_fct = as.factor("19")) %>%
	bind_rows(data)

# Bar chart of convergence proportions
convergence_info_m <- convergence_plot(data = data_m,
					  varied = "varied_parameter",
					  filter_cond = "varied_parameter != '19'",
					  convergence_max = 500000,
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

# average deviation trajectory and comparison with counterfactual for m = 10 and  m = 63 
intervention_m_10 <- deviation_plot(data = data_m, varied = "FEM", filter_cond = "varied_parameter == '10'")
ggsave("report/plots/average_intervention_m_10.png", width = 25, height = 15, units = "cm")
counterfactual_m_10 <- counterfactual_plot(data = intervention_m_10, varied = "FEM")


deviation_plot(data = data_m, varied = "FEM", filter_cond = "varied_parameter == '63'", no_col = TRUE)



# share of profitable deviations as a function of m

intervention_m <- deviation_plot(data = data_m, noplot = TRUE)
counterfactual_m <- counterfactual_plot(data = intervention_m, varied = c("varied_parameter_fct", "FEM"), noplot = TRUE)





deviation_profitability_m <- counterfactual_m %>%
	filter(metric == "profit", tau > 0) %>%
	mutate(actual_discounted = actual * 0.95^(tau - 1),
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%
	group_by(FEM, varied_parameter, run_id, agent) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%
	mutate(diff_discounted = actual_discounted - counterfactual_discounted) 

deviation_profitability_table_m <- deviation_profitability_m %>%
	mutate(agent = str_sub(agent, end = -7L)) %>%
	group_by(FEM, varied_parameter, agent) %>%
	summarize("share profitable" = mean(diff_discounted > 0)) %>%
	pivot_wider(names_from = "varied_parameter", names_prefix = "m = ", values_from = 'share profitable')


print(xtable(deviation_profitability_table_m, type = "latex"),
		file = "report/tables/share_deviation_profitability_m.tex",
		floating = FALSE,
		include.rownames = FALSE)



# load experiments with alternative zeta (excess range above monopoly price)
load("simulation_results/zeta_final/aggregated.RData")

data_zeta <- manually_optimized_alpha %>%
	mutate(varied_parameter = "1.0",
			 varied_parameter_fct = as.factor("1.0")) %>%
	bind_rows(data)

# Bar chart of convergence proportions
convergence_info_zeta <- convergence_plot(data = data_zeta,
													varied = "varied_parameter",
													convergence_max = 500000,
													runs_per_experiment = 48,
													x_lab = expression(zeta))


# proportions by zeta (over all feature extraction methods)
convergence_info_zeta %>%
	count(status, wt = value) %>%
	mutate(prop = n/sum(n))


# average Delta
zeta_delta <- point_line_plot(data = data_zeta, varied = "varied_parameter", x_lab = expression(zeta), x_log10 = FALSE)
ggsave("report/plots/zeta.png", width = 25, height = 15, units = "cm")


filter(data_zeta, !is.na(avg_profits)) %>%
	mutate(Delta = get_delta(avg_profits)) %>%	
	ggplot(aes(x = as.factor(as.numeric(varied_parameter)), y = Delta, fill = FEM)) +
	geom_hline(yintercept = c(0, 1), linetype = "dashed") +
	scale_y_continuous(expand = c(0, 0), breaks = seq(-0.5,1, by = 0.25), labels = c("-0.5", "-0,25", expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	geom_violin(draw_quantiles = 0.5, color = "grey35", scale = "width") +
	labs(x = expression(alpha), y = expression(Delta)) +
	ai_theme +
	facet_wrap(~FEM) +
	fill_dictionary +
	guides(fill = FALSE) +
	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))

data_zeta %>%
	ggplot(aes(x = as_factor(as.numeric(varied_parameter)), y = avg_prices, fill = FEM)) +
	geom_hline(yintercept = c(1.472927, 1.92498), linetype = "dashed") +
	geom_violin(draw_quantiles = 0.5, color = "grey35", scale = "width") +
	labs(x = expression(zeta), y = "average prices") +
	ai_theme +
	facet_wrap(~FEM) +
	fill_dictionary +
	guides(fill = FALSE)
ggsave("report/plots/zeta_violin_prices.png", width = 25, height = 15, units = "cm")



# average deviation trajectory for different vzeta = 0.1 and zeta = 1.5
intervention_zeta_01 <- deviation_plot(data = data_zeta, varied = "FEM", filter_cond = "varied_parameter == '0.1'")
counterfactual_plot(data = intervention_zeta_01, varied = "FEM")

intervention_zeta_15 <- deviation_plot(data = data_zeta, varied = "FEM", filter_cond = "varied_parameter == '1.5'")
counterfactual_plot(data = intervention_zeta_15, varied = "FEM")

intervention_zeta_tabular <- deviation_plot(data = data_zeta, varied = "varied_parameter", filter_cond = "FEM == 'Tabular'", no_col = TRUE)
ggsave("report/plots/average_intervention_zeta_tabular.png", width = 25, height = 15, units = "cm")


# share of profitable deviations as a function of zeta

intervention_zeta <- deviation_plot(data = data_zeta, noplot = TRUE)
counterfactual_zeta <- counterfactual_plot(data = intervention_zeta, varied = c("varied_parameter_fct", "FEM"), noplot = TRUE)

deviation_profitability_zeta <- counterfactual_zeta %>%
	filter(metric == "profit", tau > 0) %>%
	mutate(actual_discounted = actual * 0.95^(tau - 1),
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%
	group_by(FEM, varied_parameter_fct, run_id, agent) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%
	mutate(diff_discounted = actual_discounted - counterfactual_discounted) 

deviation_profitability_table_zeta <- deviation_profitability_zeta %>%
	mutate(agent = str_sub(agent, end = -7L)) %>%
	group_by(FEM, varied_parameter_fct, agent) %>%
	summarize("share profitable" = mean(diff_discounted > 0)) %>%
	pivot_wider(names_from = "varied_parameter_fct", names_prefix = "$\\zeta =$ ", values_from = 'share profitable') %>%
	select(FEM, agent, "$\\zeta =$ 0.1", "$\\zeta =$ 0.5", "$\\zeta =$ 1.0", "$\\zeta =$ 1.5")


print(xtable(deviation_profitability_table_zeta, type = "latex"),
		file = "report/tables/share_deviation_profitability_zeta.tex",
		floating = FALSE,
		sanitize.colnames.function = identity,
		include.rownames = FALSE)



# 7.4 - Gamma & Appendix B.5 ------------------------------------------------------------------

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


gamma_delta %>%
	filter(FEM %in% c("Tabular", "Tile Coding")) %>%
	arrange(desc(Delta))

# distribution of average prices 
data_gamma %>%
	filter(!is.na(avg_prices)) %>%
	ggplot(aes(x = fct_rev(varied_parameter_fct), y = avg_prices, fill = FEM)) +
	geom_hline(yintercept = c(1.472927, 1.92498), linetype = "dashed") +
	geom_violin(draw_quantiles = 0.5, color = "grey35", scale = "width") +
	labs(x = expression(gamma), y = "average price") +
	ai_theme +
 	coord_cartesian(ylim = c(1.3, 2)) +
	facet_wrap(~FEM) +
	fill_dictionary +
	guides(fill = FALSE)
ggsave("report/plots/gamma_violin_price.png", width = 25, height = 15, units = "cm")


# average deviation trajectory for tabular learning
intervention_gamma_tabular <- deviation_plot(data = data_gamma, varied = "varied_parameter_fct",
														  filter_cond = "FEM == 'Tabular' & varied_parameter != '0.95'",
														  no_col = TRUE)
ggsave("report/plots/average_intervention_gamma_tabular.png", width = 25, height = 15, units = "cm")

# price comparison with counterfactual (adhering to learned strategies)
counterfactual_plot(data = intervention_gamma_tabular, varied = "varied_parameter_fct", no_col = TRUE)

# average deviation trajectory for gamma = 0, 0.9 & 1
deviation_plot(data = data_gamma, varied = "FEM", filter_cond = "varied_parameter == '0'")  # no punishment throughout methods
deviation_plot(data = data_gamma, varied = "FEM", filter_cond = "varied_parameter == '0.8'")    # clearest punishment for tabular learning, not for other methods
deviation_plot(data = data_gamma, varied = "FEM", filter_cond = "varied_parameter == '0.99'")    # surprsinginly consistent with previous results

deviation_plot(data = filter(data_gamma, !(varied_parameter %in% c("0", "1", "0.25"))), varied = "varied_parameter", filter_cond = "FEM == 'Tabular'", no_col = TRUE)


# share of profitable deviations as a function of gamma
intervention_gamma <- deviation_plot(data = data_gamma, noplot = TRUE)
counterfactual_gamma <- counterfactual_plot(data = intervention_gamma, varied = c("varied_parameter_fct", "FEM"), noplot = TRUE)

deviation_profitability_gamma <- counterfactual_gamma %>%
	filter(metric == "profit", tau > 0) %>%
	mutate(actual_discounted = actual * 0.95^(tau - 1),
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%
	group_by(FEM, varied_parameter_fct, run_id, agent) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%
	mutate(diff_discounted = actual_discounted - counterfactual_discounted) 

deviation_profitability_gamma %>%
	mutate(agent = str_sub(agent, end = -7L)) %>%
	group_by(FEM, varied_parameter_fct, agent) %>%
	summarize("share profitable" = mean(diff_discounted > 0)) %>%
	pivot_wider(names_from = "varied_parameter_fct", names_prefix = "$\\gamma =$ ", values_from = 'share profitable') %>%
	select(FEM, agent, "$\\gamma =$ 0", "$\\gamma =$ 0.5", "$\\gamma =$ 0.8", "$\\gamma =$ 0.95", "$\\gamma =$ 0.99")




# 7.5 Alternative Algorithms & Appendix B.6 -----------------------


# Load Tree backup experiments

load("simulation_results/tree_backup_final/aggregated.RData")

# manually associate the optimized values of alpha with the runs
data_tb <- data %>%
	mutate(
		varied_parameter = case_when(
			FEM == "Separate Polynomials" ~ "1e-06",
			FEM == "Polynomial Tiles" ~ "1e-08",
			FEM == "Tile Coding" ~ "0.001",
			FEM == "Tabular" ~ "0.1",
			),
		varied_parameter_fct = as_factor(varied_parameter)
	)


# all but a single run converged (warning message can be ignored)
convergence_info_tb <- convergence_plot(data = data,
												 varied = "varied_parameter",
												 convergence_max = 500000,
												 runs_per_experiment = 48,
												 x_lab = "" )

# no variation --> bar label is NA
convergence_info_tb %>%
	count(status, wt = value) %>%
	mutate(prop = n/sum(n))

data_tb %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	filter(between(Delta, 0.95, 0.99)) %>%
	unnest(intervention)


alpha_tb <- point_line_plot(data = data_tb, varied = "varied_parameter", x_lab = expression(alpha), x_log10 = TRUE)

data_tb %>%
	filter(convergence < 500000) %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	ggplot(aes(x = FEM, y = Delta, fill = FEM)) +
	geom_hline(yintercept = c(0, 1), linetype = "dashed") +
	scale_y_continuous(expand = c(0, 0), breaks = seq(-0.5,1, by = 0.25), labels = c("-0.5", "-0,25", expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	geom_violin(draw_quantiles = 0.5, color = "grey35", scale = "width") +
	labs(x = " ", y = expression(Delta)) +
	ai_theme +
	fill_dictionary +
	guides(fill = FALSE) +
	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
ggsave("report/plots/tb_violin.png", width = 25, height = 10, units = "cm")



# Learning Phase Trajectory

data_tb %>%
	select(-intervention, -avg_profits) %>%
	unnest(outcomes) %>%
	pivot_longer(cols = c(price, Delta), names_to = "metric") %>%
	mutate(p_n = ifelse(metric == "Delta", 0, 1.472927),
			 p_m = ifelse(metric == "Delta", 1, 1.92498)) %>%
	ggplot(aes(x = t_group * t_grouping, y = value, group = interaction(FEM, run_id, varied_parameter), col = FEM)) +
	geom_hline(aes(yintercept = p_n), linetype = "dashed") +	
	geom_hline(aes(yintercept = p_m), linetype = "dashed") +	
	geom_line() +
	facet_wrap(~metric, ncol =1, scales = "free_y") +
	ai_theme +
	scale_x_continuous(labels = scales::comma) +
	color_dictionary +
	labs(x = "t", y = " ")


# Deviations


intervention_tb <- deviation_plot(data = data_tb, varied = "FEM")
ggsave("report/plots/average_intervention_tb.png", width = 25, height = 15, units = "cm")


counterfactual_tb <- counterfactual_plot(data = intervention_tb, varied = "FEM")
ggsave("report/plots/intervention_boxplot_tb.png", width = 25, height = 22, units = "cm")


# identify and display  polynomial tiling runs where cheated agent respondedwith price cut at tau = 2
(poly_tiling_deviations_matching <- filter(counterfactual_tb, FEM == "Polynomial Tiles", tau == "2", metric == "price", agent == "non deviating agent", diff < 0))

counterfactual_tb %>%
	filter(FEM == "Polynomial Tiles", run_id %in% poly_tiling_deviations_matching$run_id, tau > -2, metric == "price") %>%
	mutate(p_n = ifelse(metric == "profit", 0.223, 1.472927),
			 p_m = ifelse(metric == "profit", 0.337, 1.92498),
			 run_id = str_pad(run_id, 2, pad = "0")) %>%
	ggplot(aes(x = tau, y = actual, shape = agent, linetype = agent)) +
	geom_hline(aes(yintercept = p_n), linetype = "dashed") +	
	geom_hline(aes(yintercept = p_m), linetype = "dashed") +geom_line() +
	geom_point() +
	facet_wrap(~run_id, scales = "free_y") +
	scale_x_continuous(labels = scales::comma) +
	ai_theme +
	labs(x  = expression(tau), y = "")






# On Policy 

load("simulation_results/on_policy_final/aggregated.RData")

# manually associate the optimized values of alpha with the runs
data_op <- data %>%
	mutate(
		varied_parameter = case_when(
			FEM == "Separate Polynomials" ~ "1e-06",
			FEM == "Polynomial Tiles" ~ "1e-08",
			FEM == "Tile Coding" ~ "0.001",
			FEM == "Tabular" ~ "0.1",
		),
		varied_parameter_fct = as_factor(varied_parameter)
	)


# all runs converged (warning message can be ignored)
convergence_info_op <- convergence_plot(data = data,
													 varied = "varied_parameter",
													 convergence_max = 500000,
													 runs_per_experiment = 48,
													 x_lab = "" )
# no variation --> bar label is NA
convergence_info_op %>%
	count(status, wt = value) %>%
	mutate(prop = n/sum(n))


alpha_op <- point_line_plot(data = data_op, varied = "varied_parameter", x_lab = expression(alpha), x_log10 = TRUE)

data_op %>%
	filter(convergence < 500000) %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	ggplot(aes(x = FEM, y = Delta, fill = FEM)) +
	geom_hline(yintercept = c(0, 1), linetype = "dashed") +
	scale_y_continuous(expand = c(0, 0), breaks = seq(-0.5,1, by = 0.25), labels = c("-0.5", "-0,25", expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	geom_violin(draw_quantiles = 0.5, color = "grey35", scale = "width") +
	labs(x = " ", y = expression(Delta)) +
	ai_theme +
	fill_dictionary +
	guides(fill = FALSE) +
	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
ggsave("report/plots/op_violin.png", width = 25, height = 10, units = "cm")



# Learning Phase Trajectory 

# prices more consistently between benchmarks and price trajectory appears a little more stable?
data_op %>%
	select(-intervention, -avg_profits) %>%
	unnest(outcomes) %>%
	pivot_longer(cols = c(price, Delta), names_to = "metric") %>%
	mutate(p_n = ifelse(metric == "Delta", 0, 1.472927),
			 p_m = ifelse(metric == "Delta", 1, 1.92498)) %>%
	ggplot(aes(x = t_group * t_grouping, y = value, group = interaction(FEM, run_id, varied_parameter), col = FEM)) +
	geom_hline(aes(yintercept = p_n), linetype = "dashed") +	
	geom_hline(aes(yintercept = p_m), linetype = "dashed") +	
	geom_line() +
	facet_wrap(~metric, ncol =1, scales = "free_y") +
	ai_theme +
	scale_x_continuous(labels = scales::comma) +
	color_dictionary +
	labs(x = "t", y = " ")


# Deviations

# note that due to the delayed learning in on-policy learning, the deviation takes place at tau = 2 and a (potential) response at tau = 3
intervention_op <- deviation_plot(data = data_op, varied = "FEM")


counterfactual_op <- counterfactual_plot(data = intervention_op, varied = "FEM")


(poly_tiling_deviations_matching_op <- filter(counterfactual_op, FEM == "Polynomial Tiles", tau == "3", metric == "price", agent == "non deviating agent", diff < 0))

counterfactual_op %>%
	filter(FEM == "Polynomial Tiles", run_id %in% poly_tiling_deviations_matching_op$run_id, tau > -2, metric == "price") %>%
	mutate(p_n = ifelse(metric == "profit", 0.223, 1.472927),
			 p_m = ifelse(metric == "profit", 0.337, 1.92498),
			 run_id = str_pad(run_id, 2, pad = "0")) %>%
	ggplot(aes(x = tau, y = actual, shape = agent, linetype = agent)) +
	geom_hline(aes(yintercept = p_n), linetype = "dashed") +	
	geom_hline(aes(yintercept = p_m), linetype = "dashed") +geom_line() +
	geom_point() +
	facet_wrap(~run_id, scales = "free_y") +
	scale_x_continuous(labels = scales::comma) +
	ai_theme +
	labs(x  = expression(tau), y = "")




# 7.6 differential reward setting and Appendix B.7 ------------------------------------------------------------


load("simulation_results/Upsilon_final/aggregated.RData")
data_upsilon <- data

# invoke function from above to create stacked plot with convergence info
convergence_info_upsilon <- convergence_plot(data = data_upsilon,
												 varied = "varied_parameter",
												 convergence_max = 500000,
												 runs_per_experiment = 48,
												 x_lab = expression(upsilon))
ggsave("report/plots/converged_upsilon.png", width = 25, height = 15, units = "cm")

# convergence by FEM
convergence_info_upsilon %>%
	filter(status != "failed") %>%
	group_by(FEM) %>%
	count(status, wt = value) %>%
	mutate(prop = n/sum(n))

data_upsilon %>%
	filter(convergence < 500000) %>%
	ggplot(aes(x = convergence, y = stat(density), col = FEM)) +
	geom_freqpoly(size = 1.5, binwidth = 8000, alpha = 0.7) +
	scale_x_continuous(limits = c(0, 500000), labels = scales::comma) +
	color_dictionary +
	labs(x = "t") +
	ai_theme +
	guides(colour = guide_legend(override.aes = list(alpha = 1)))
ggsave("report/plots/convergence_at_upsilon.png", width = 25, height = 12, units = "cm")	



# also, a tendency towards shorter cycles
data_upsilon %>%
	ggplot(aes(x = as.factor(cycle_length), fill = FEM)) +
	geom_bar(position = "stack", na.rm = TRUE) +
	scale_x_discrete(na.translate = FALSE) +
	ai_theme +
	fill_dictionary +
	labs(x = "cycle length", y = "")


# Vary Upsilon


upsilon_delta <- point_line_plot(data = data_upsilon, varied = "varied_parameter", x_lab = expression(upsilon), x_log10 = TRUE)
ggsave("report/plots/upsilon.png", width = 25, height = 15, units = "cm")


data_upsilon %>%
	mutate(Delta = get_delta(avg_profits)) %>%
	ggplot(aes(x = fct_rev(varied_parameter_fct), y = Delta, fill = FEM)) +
	geom_hline(yintercept = c(0, 1), linetype = "dashed") +
	scale_y_continuous(expand = c(0, 0), breaks = seq(-0.5,1, by = 0.25), labels = c("-0.5", "-0,25", expression(Delta[n]), "0.25", "0.5", "0.75", expression(Delta[m]))) +
	geom_violin(draw_quantiles = 0.5, color = "grey35", scale = "width") +
	labs(x = expression(upsilon), y = expression(Delta)) +
	ai_theme +
	facet_wrap(~FEM) +
	fill_dictionary +
	guides(fill = FALSE) +
	theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
ggsave("report/plots/upsilon_violin.png", width = 25, height = 15, units = "cm")




# Deviations


# deviation trajectory for upsilon = 0.005 and 0.1 
intervention_upsilon_005 <- deviation_plot(data = data_upsilon, varied = "FEM", filter_cond = "varied_parameter == '0.005'")
counterfactual_plot(data = intervention_upsilon_005, varied = "FEM")
ggsave("report/plots/intervention_boxplot_upsilon_005.png", width = 25, height = 22, units = "cm")

intervention_upsilon_1 <- deviation_plot(data = data, varied = "FEM", filter_cond = "varied_parameter == '0.1'")
counterfactual_plot(data = intervention_upsilon_1, varied = "FEM")

# deviation trajectory for, respectively, tabular learning and tile coding
deviation_plot(data = data, varied = "varied_parameter", filter_cond = "FEM == 'Tabular'", no_col = TRUE)

deviation_plot(data = data, varied = "varied_parameter", filter_cond = "FEM == 'Tile Coding'", no_col = TRUE)


# share of profitable deviations as a function of upsilon and feature method
intervention_upsilon <- deviation_plot(data = data_upsilon, noplot = TRUE)
counterfactual_upsilon <- counterfactual_plot(data = intervention_upsilon, varied = c("varied_parameter_fct", "FEM"), noplot = TRUE)

deviation_profitability_upsilon <- counterfactual_upsilon %>%
	filter(metric == "profit", tau > 0) %>%
	mutate(actual_discounted = actual * 0.95^(tau - 1),
			 counterfactual_discounted = counterfactual * 0.95^(tau - 1)) %>%
	group_by(FEM, varied_parameter_fct, run_id, agent) %>%
	summarize_at(.vars = c("actual_discounted", "counterfactual_discounted"), .funs = sum) %>%
	mutate(diff_discounted = actual_discounted - counterfactual_discounted) 

# share of profitable deviations by FEM
deviation_profitability_upsilon %>%
	mutate(agent = str_sub(agent, end = -7L)) %>%
	group_by(FEM, agent) %>%
	summarize("share profitable" = mean(diff_discounted > 0))

# share of profitable deviations by FEM and upsilon
deviation_profitability_upsilon %>%
	mutate(agent = str_sub(agent, end = -7L)) %>%
	group_by(FEM, varied_parameter_fct, agent) %>%
	summarize("share profitable" = mean(diff_discounted > 0)) %>%
	pivot_wider(names_from = "varied_parameter_fct", names_prefix = "$\\upsilon =$ ", values_from = 'share profitable')


