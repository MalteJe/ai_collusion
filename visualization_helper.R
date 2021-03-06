# Theme ------------------------------------------------------------------
# visual specifications

ai_theme <- theme(
	axis.title = element_text(size = 14, margin = margin(0.3, 0.3, -1, 0, unit = "mm")),
	axis.text = element_text(size = 11),
	panel.background = element_rect(fill = "white", color = NA),
	panel.border = element_rect(fill = NA, size = 0.2),
	panel.grid.major.x = element_line(color = "grey85", size = 0.2),
	panel.grid.major.y = element_line(color = NA),
	panel.grid.minor.x = element_line(color = NA),
	panel.grid.minor.y = element_line(color = NA),
	plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
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
	"Tabular" = "black",
	"Tile Coding" = "#4DBBD5",
	"Polynomial Tiles" = "#E64B35",
	"Separate Polynomials" = "#F39B7FFF"
)

fill_dictionary <- scale_fill_manual(values = color_dictionary)
color_dictionary <- scale_color_manual(values = color_dictionary)


# function to line & scatter-plot with Delta on the y-axis as a function of feature extraction method and specified x-axis
point_line_plot <- function(data, varied, filter_cond = "TRUE", x_lab, x_log10 = FALSE, with_authors = FALSE) {
	
	# extraction variations
	variations <- sort(unique(data[eval(varied)] %>%
									  	pull(varied) %>%
									  	as.numeric()))
	
	# get average Delta by FEM and requested variation
	res <- filter(data, !is.na(avg_profits)) %>%
		group_by_at(vars(all_of(c("FEM", varied)))) %>%
		summarize(avg_profits = mean(avg_profits)) %>%
		mutate(Delta = get_delta(avg_profits)) %>%
		filter(!! parse_expr(filter_cond))   # pass filter condition
	
	# creates requested plot as output
	out <- res %>%
		ggplot(aes(x = as.double(!!sym(varied)), y = Delta, col = FEM)) +
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
	
	
	# add reference points for Calvano et al. (only works with varying alpha)
	if (with_authors) {
		
		reference <- tibble(
			FEM = "Tabular",
			author = "Calvano et al.\n(Q-Learning)",
			varied_parameter = "0.1",
			Delta = c(0.76)
		)
		
		out <- out +
			geom_point(data = reference,
						  color = "black",
						  shape = 4,
						  size = 5) +
			geom_text(data = reference,
						  aes(
						  	x = as.numeric(varied_parameter),
						  	y = Delta,
						  	label = author),
						  color = "black",
						  nudge_x = -0.8)
		
	}
	
	# apply log10 scale if requested
	if (x_log10) {
		out <- out + scale_x_log10(minor_breaks = variations, breaks = variations)
	} else {
		out <- out + scale_x_continuous(minor_breaks = variations, breaks = variations)
	}
	

	
	# print plot and reutrn 'res'
	print(out)
	return(res)
}




# function to get stacked bar plot with varied parameter on y axis and a count of converged/non-converged/failed runs
convergence_plot <- function(data, varied, filter_cond = "TRUE", convergence_max, runs_per_experiment, x_lab) {
	
	# get maximum convergence length (potentially by varied parameter)
	if(length(convergence_max) == 1) {
		data$convergence_threshold <- convergence_max
	} else {
		data <- left_join(data, convergence_max, by = "varied_parameter")
	}
	
	
	res <- data %>%
		# identify converged runs and 'complete' with implicitly missing runs (without files in folder)
		mutate(converged = (convergence < convergence_threshold)) %>%
		complete(FEM, varied_parameter) %>%
		
		# filter anything out (relevant depending on experiment)
		filter(!! parse_expr(filter_cond)) %>%
		
		# summarize in converged, not-converged and failed runs & wrangle in long format 
		group_by(FEM, varied_parameter) %>%
		summarize(failed = runs_per_experiment - n_distinct(run_id, na.rm = TRUE),
					 converged = sum(converged)) %>%
		ungroup() %>%
		mutate(`not converged` = runs_per_experiment - failed - converged) %>%
		pivot_longer(cols = c("converged", "not converged" ,"failed"), names_to = "status") %>%
		mutate(status = forcats::fct_relevel(status, "failed", "not converged", "converged"))
	
	# create output plot and print
	out <- res %>%
		ggplot(aes(x = as_factor(as.numeric(!!sym(varied))), y = value, fill = status)) +
		geom_col(position = "stack") +
		facet_wrap(~FEM) +	fill_dictionary +
		coord_flip() +
		labs(y = "runs", x = x_lab) +
		ai_theme +
		theme(axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5))
	
	print(out)
	
	# return results
	return(res)
}




# function to unnest deviation data and plot average prices around deviation of both players
deviation_plot <- function(data, varied, filter_cond = "TRUE", tau_min = -5, noplot = FALSE, no_col = FALSE) {
	
	# parse filter condition and unndest deviation experiment data
	res <- data  %>%
		filter(!! parse_expr(filter_cond)) %>%
		select(-outcomes, -successful, -convergence, -avg_profits) %>%
		unnest(intervention)
	
	# if no color variation requested --> overwrite color dictionary (every variation in black)
	if(no_col) {
		vals <- unique(res[[varied]])
		assignments <- set_names(rep("black", length(vals)), vals)
		color_dictionary <- scale_color_manual(values = assignments)
		}
	
	# only create plot if requested
	if(!noplot) {
		
		# create output plots
		out <- res %>%
			
			# group by period and varied parameter to take mean over all runs by player
			group_by(!!sym(varied), tau) %>%    
			summarize("deviating agent" = mean(price_1),
						 "non deviating agent" = mean(price_2)) %>%
			
			# wrangle in long format and filter for specified time range
			pivot_longer(cols = c("deviating agent", "non deviating agent"), names_to = "price") %>%
			filter(tau > tau_min) %>%
			ggplot(aes(x = tau, y = value, linetype = price, shape = price, col = !!sym(varied))) +
			
			# reference lines
				geom_hline(yintercept = c(1.47, 1.93), linetype = "dashed") +          
				geom_vline(xintercept = 0, linetype = "dotted") +                      
				geom_point(size = 3) +
				geom_line(size = 1) +
				scale_linetype_manual(values = c("solid", "dotted")) +
				facet_wrap(vars(!!sym(varied)), nrow = 2) +
				labs(x = expression(tau), y = "average price") +
				ai_theme +
				color_dictionary +
				guides(col = FALSE)
		
		# printout
		print(out)
	}
	
	
	return(res)
}


# helper function to extend whiskers in boxplots to full range
full_whisker <- function(x) {
	r <- quantile(x, probs = c(0, 0.15, 0.5, 0.85, 1))
	names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
	r
}

# function to compare prices (or profits) at deviation and thereafter
counterfactual_plot <- function(data, varied, filter_cond = "TRUE", y_axis_variable = "price", noplot = FALSE, no_col = FALSE) {
	
	# create counterfactual if both agents had folloed their strategy
	res <- data  %>%
		filter(!! parse_expr(filter_cond),
				 !is.na(cycle_length)) %>%    # remove not-converged runs because the counterfactual is not clear
		pivot_longer(cols = c("price_1", "price_2", "profit_1", "profit_2"), names_to = "obs", values_to = "actual") %>%
		group_by_at(c(varied, "run_id", "obs")) %>%
		mutate(replacement_pos = ifelse(tau <=0, NA, row_number() - cycle_length * (1 + (row_number() - 11) %/% cycle_length))) %>%
		mutate(counterfactual = ifelse(is.na(replacement_pos), actual, actual[replacement_pos]),
				 diff = actual - counterfactual) %>%
		separate(col = obs, into = c("metric", "agent"), sep = "_") %>%
		mutate(agent = ifelse(agent == "1", "deviating agent", "non deviating agent"))
	
	# if no color variation requested --> overwrite color dictionary (every variation in black)
	if(no_col) {
		vals <- unique(res[[varied]])
		assignments <- set_names(rep("black", length(vals)), vals)
		fill_dictionary <- scale_fill_manual(values = assignments)
	}
	
	# only create plot if requested
	if(!noplot) {
		
		# create deviation plot
		out <- res %>%
			filter(tau >= 0, metric == y_axis_variable) %>%
			ggplot(aes(x = as.factor(tau), y = diff, fill = !!sym(varied))) +
				stat_summary(fun.data = full_whisker, geom = "errorbar", width = 0.5) +
				stat_summary(fun.data = full_whisker, geom = "boxplot", col = "gray35") +
				facet_grid(rows = vars(!!sym(varied)), cols = vars(agent)) +
				ai_theme +
				fill_dictionary +
				labs(x = expression(tau), y = str_c(y_axis_variable, " difference")) +
				guides(fill = FALSE)
		print(out)
	}
	
	
	return(res)
}


