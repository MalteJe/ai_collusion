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
		print(path)
		
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
	str_subset("aggregated", negate = TRUE); print(head(filenames)); print(length(filenames))

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
	
	








