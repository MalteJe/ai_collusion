library(purrr)
library(tidyquant)
library(tidyverse)

source("C:/Users/psymo/OneDrive/Studium/a_projects/ai_collusion/environment.R")


# Action Space ------------------------------------------------------------

n <- 2
zeta <- 0.1
p_n <- 1.4728
p_m <- 1.92498
m <- 15

single_run <- function(n, zeta, m, ...) {
	
	# calculate discrete set of prices
	p_n <- nash_prices(n, ...)
	p_m <- optimize_joint_profits(n, ...) %>% mean()
	available_prices <- seq(from = p_n - zeta*(p_m - p_n), to = p_m + zeta * (p_m - p_n), length.out = m)

	return(available_prices)
}


single_run(n = 2,
			  zeta = 0.1,
			  m = 80,
			  c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)

# available_prices <- seq(from = p_n -zeta*(p_m - p_n), to = p_m + zeta * (p_m - p_n), length.out = m)

# Reward list (generalize for n players TO DO)

R <- cross2(available_prices, available_prices) %>%
	map(as.numeric) %>% 
	map(calculate_profits, c = c(1,1), a = c(2,2), a_0 = 0, mu = 0.25)


# Initialize Q-matrix
delta <- 0.95

init_profits <- function(p1, p2) {
	calculate_profits(p = c(p1,p2),
							c = c(1,1),
							a = c(2,2),
							a_0 = 0, mu = 0.25)[1]
}

init_Q_entry <- function(i, delta) {
	pis <- map_dbl(available_prices, init_profits, p1 = available_prices[i])
	sum(pis)/((1-delta) * length(available_prices))
}

initial_Q_values <- map_dbl(1:length(available_prices), init_Q_entry, delta = delta)

init_Q <- matrix(rep(initial_Q_values, times = 15^2), ncol = 15, byrow = TRUE)
Qs <- list(init_Q, init_Q)
str(Qs)

# Exploration Rate --------------------------------------------------------

TT <- 1200000
epsilon <- exp(-(5*10^-6)*1:TT)
tail(epsilon)
alpha <- 0.1



# Setup trackers ----------------------------------------------------------
t <- 1  # tracks time
outcomes <- matrix(NA, nrow = TT, ncol = n * 2, dimnames = list(NULL, c("price1", "price2", "profit1", "profit2")))
dim(outcomes)

# Setup initial round -----------------------------------------------------
set.seed(1234567)
(initial_actions <- sample(1:m, n, replace = TRUE))

shifter <- function(n = 1, x) {
	if (n == 0) x else c(tail(x, -n), head(x, n))
}


get_state_id <- function(action_ids) {   # needs to be adjusted for n > 3!!!
	(action_ids[1] - 1) * m + action_ids[2]
}

s_t <- map(0:(n-1), shifter, x = initial_actions) %>%
	map_dbl(get_state_id)


# function to choose action based on e-greedy policy
select_action_id <- function(Q, action_id, epsilon) {
	
	# choose action based on current policy
	# draw from binomial distribution to determine whether to explore or exploit
	if(rbinom(n = 1, size = 1, prob = epsilon)) {
		
		# exploration: choose random action
		selected_action_id <- sample(1:m, 1)
	} else {
		
		# exploitation: choose optimal action
		selected_action_id <- which.max(Q[action_id,])
	}
	return(selected_action_id)
}

go <- function() {
while (t < TT) {
	
	# select actions from all players based on current policies
	selected_action_ids <- map2_dbl(.x = Qs,
											 .y = s_t,
											 .f = select_action_id,
											 epsilon = epsilon[t])
	
	# get price according to action's id
	selected_actions <- available_prices[selected_action_ids]
	
	# retrieve profits from list
	r <- R[[selected_action_ids[1] + (selected_action_ids[2]-1) * m]]
	
	# observe next stage status (i.e. collect price choice id's)
	s_t2 <- map(0:(n-1), shifter, x = selected_action_ids) %>%
		map_dbl(get_state_id)
	
	# update Q-matrix entries
	for (a in seq_along(Qs)) {
		# row <- Qs[[a]][s_t[a], ]
		# old_estimate <- Qs[[a]][s_t[a], selected_action_ids[a]]
		# re <- r[a]
		# new_state <- Qs[[a]][s_t2[a],]
		# optimal <- max(Qs[[a]][s_t2[a],])
		# new_estimate <- (1-alpha) * old_estimate + alpha * (re + delta * optimal)
		Qs[[a]][s_t[a], selected_action_ids[a]] <- (1-alpha) * Qs[[a]][s_t[a], selected_action_ids[a]] + alpha * (r[a] + delta * max(Qs[[a]][s_t2[a],]))
	}
	
	# record prices and profits
	outcomes[t,] <- c(selected_actions, r)
	
	# print informative message every 100,000 timesteps
	if (t %% 100000 == 0) {print(t)}
	
	# update state & timer
	s_t <- s_t2
	t <- t + 1
}
	return(outcomes)
}

single_simulation_outcomes <- go()
tail(single_simulation_outcomes)

metrics <- dimnames(single_simulation_outcomes)[[2]]
as_tibble(single_simulation_outcomes) %>%
	mutate(t = row_number(),
			 t_group = (t-1) %/% 2000 + 1) %>%
	group_by(t_group) %>%
	summarize_at(metrics, mean) %>%
	pivot_longer(cols = metrics, names_to = "type", values_to = "value") %>%
	ggplot(aes(x = t_group, y = value)) +
		geom_line() +
		facet_wrap(~type, nrow = 3, scales = "free_y") +
	theme_tq()



as_tibble(single_simulation_outcomes) %>%
	mutate(t = row_number()) %>%
	filter(t > 1199500) %>%
	pivot_longer(cols = metrics, names_to = "type", values_to = "value") %>%
	ggplot(aes(x = t, y = value)) +
	geom_line() +
	facet_wrap(~type, nrow = 3, scales = "free_y")


calculate_profits(c(available_prices[8], available_prices[14]),
						c = c(1,1),
						a = c(2,2),
						a_0 = 0, mu = 0.25)

init_Q[0:14 * 15 + 14,]
Q[0:14 * 15 + 14,]
head(profits1)
head(profits2)


temp <- function(p1, p2) {
	res <- calculate_profits(p = c(p1,p2),
							c = c(1,1),
							a = c(2,2),
							a_0 = 0, mu = 0.25)
	-sum(res[1])
}
	
temp(1.5, 1.5)
	

optim(par = 1.0, p2 = 1.4, fn = temp, method = "BFGS")
