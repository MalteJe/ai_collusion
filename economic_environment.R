# Economic model (simultaneous, differentiated Bertrand) ----------------------------------------------------------

# enumerator_i <- function(p, a, mu) {
# 	exp((a-p)/mu)
# }
# 
# outside <- function(a_0, mu) {exp(a_0/mu)}
# 
# calculate_demands <- function(p, a, a_0, mu) {
# 	en <- map2_dbl(.x = p,
# 						.y = a,
# 						.f = enumerator_i,
# 						mu = mu)
# 	out <- outside(a_0, mu)
# 	den <- sum(en, out)
# 	q <- en/den
# 	return(q)
# }

calculate_demands <- function(p, a, a_0, mu) {
	enumerator <- exp((a-p)/mu)
	outside <- exp(a_0/mu)
	denominator <- sum(enumerator, outside)
	return(enumerator/denominator)
}



# calculate_profits <- function(p, c, ...) {
# 	q <- calculate_demands(p, ...)
# 	pi <- (p - c) * q
# 	return(pi)
# }

calculate_profits <- function(p, c, ...) {
	q <- calculate_demands(p, ...)
	return((p - c) * q)
}


# helpers to calculate initial Q matrix

# init_profits <- function(p1, p2, ...) {
# 	calculate_profits(p = c(p1,p2), ...)[1]
# }
# 
# init_Q_entry <- function(i, delta, available_prices, ...) {
# 	pis <- map_dbl(available_prices, init_profits, p1 = available_prices[i], ...)
# 	sum(pis)/((1-delta) * length(available_prices))
# }
# 


# numerically optimize joint profits with respect to both prices
joint_profits_helper <- function(p, ...) {
	res <- calculate_profits(p = p, ...)
	-sum(res)
}

optimize_joint_profits <- function(...) {
	optim(par = rep(0, 2), fn = joint_profits_helper,
		...,
		control = list(reltol = 1e-12))$par %>%
		mean()
}


# numerically optimize best response to fixed competitor price
best_response_helper <- function(p, p_, ...) {
	res <- calculate_profits(p = c(p, p_), ...)
	-res[1]
}


best_response <- function(p_, n, ...) {
	suppressWarnings(optim(par = 0, fn = best_response_helper,
			p_ = p_,
			...,
			control = list(reltol = 1e-12))$par)
}

# numerically obtain (mututally best responses) to obtain Nash equilibrium

# nash_helper <- function(p, ...) {
# 	res <- best_response(p_ = p, ...)
# 	abs(res - p)
# }

nash_helper <- function(p, c, a, a_0, mu) {
	br <- c + mu/(1 - (2 + exp((a_0 + p - a)/mu))^-1)
	return(abs(br - p))
}

nash_prices <- function(...) {
	suppressWarnings(optim(par = 0, nash_helper,
			...,
			control = list(reltol = 1e-12))$par)
}
