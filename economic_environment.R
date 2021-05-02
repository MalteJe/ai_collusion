# Economic model (simultaneous, differentiated Bertrand) ----------------------------------------------------------

# calculate_demands calculates the multinominal logit demand as a function of prices and parameters (a, a_0, mu)
calculate_demands <- function(p, a, a_0, mu) {
	enumerator <- exp((a-p)/mu)
	outside <- exp(a_0/mu)
	denominator <- sum(enumerator, outside)
	return(enumerator/denominator)
}

# calculate_profits calculates the profits in the multinominal logit demand model as a function of prices and parameters
calculate_profits <- function(p, c, ...) {
	q <- calculate_demands(p, ...)
	return((p - c) * q)
}


# Optimize joint profits --------------------------------------------------

# joint_profits_helper calculates (the negative of) joint profits assuming both firms charge the same price
joint_profits_helper <- function(p, ...) {
	res <- calculate_profits(p = rep(p, 2), ...)
	-sum(res)
}

# optimize_joint_profits numerically optimizes price with respect to joint profits 
optimize_joint_profits <- function(...) {
	suppressWarnings(optim(par = 0, fn = joint_profits_helper,
		...,
		control = list(reltol = 1e-12))$par)
}


# Best response -----------------------------------------------------------

# best_response_helper calculates the (negative of the) profits of firm 1 given a price in response to firm 2's price
best_response_helper <- function(p, p_, ...) {
	res <- calculate_profits(p = c(p, p_), ...)
	-res[1]
}

# best_response numerically optimizes (with respect to profits) the price of firm 1 given a price of firm 2
best_response <- function(p_, ...) {
	suppressWarnings(optim(par = 0, fn = best_response_helper,
			p_ = p_,
			...,
			control = list(reltol = 1e-12))$par)
}


# Nash equilibrium --------------------------------------------------------

# nash_helper checks the distance of p to the unique equilibrium, where the result of the formula is equal to the input price. (see Anderson, de Palma 1992)
nash_helper <- function(p, c, a, a_0, mu) {
	res <- c + mu/(1 - (2 + exp((a_0 + p - a)/mu))^-1)   # n is hard-coded as '2' for this simulation
	return(abs(res - p))
}

# nash_prices numerically optimizes p such that the condition for a unique equilbrium is (approximately) satisfied
nash_prices <- function(...) {
	suppressWarnings(optim(par = 0, nash_helper,
			...,
			control = list(reltol = 1e-12))$par)
}