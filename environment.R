# Economic model (simultaneous, differentiated Bertrand) ----------------------------------------------------------

enumerator_i <- function(p, a, mu) {
	exp((a-p)/mu)
}

outside <- function(a_0, mu) {exp(a_0/mu)}

calculate_demands <- function(p, a, a_0, mu) {
	en <- map2_dbl(p, a, enumerator_i, mu = mu)
	out <- outside(a_0, mu)
	den <- sum(en, out)
	q <- en/den
	return(q)
}


calculate_profits <- function(p, c, ...) {
	q <- calculate_demands(p, ...)
	pi <- (p - c) * q
	return(pi)
}


# helpers to calculate initial Q matrix

init_profits <- function(p1, p2, ...) {
	calculate_profits(p = c(p1,p2), ...)[1]
}

init_Q_entry <- function(i, delta, available_prices, ...) {
	pis <- map_dbl(available_prices, init_profits, p1 = available_prices[i], ...)
	sum(pis)/((1-delta) * length(available_prices))
}



# numerically optimize joint profits with respect to both prices

joint_profits_helper <- function(p, ...) {
	res <- calculate_profits(p = p, ...)
	-sum(res)
}

optimize_joint_profits <- function(n, ...) {
	optim(par = rep(0, n), fn = joint_profits_helper,
		...,
		control = list(reltol = 1e-12))$par
}


# numerically optimize best response to fixed competitor price

best_response_helper <- function(p, p_, n, ...) {
	res <- calculate_profits(p = c(p, rep(p_, n-1)), ...)
	-res[1]
}


best_response <- function(p_, n, ...) {
	suppressWarnings(optim(par = 0, fn = best_response_helper,
			p_ = p_,
			n = n,
			...,
			control = list(reltol = 1e-12))$par)
}


# numerically minimize difference between best response and competitor price (works only in symmetric case)

nash_helper <- function(p, ...) {
	res <- best_response(p_ = p, ...)
	abs(res - p)
}


nash_prices <- function(n, ...) {
	suppressWarnings(optim(par = 0, nash_helper,
			n = 2,
			...,
			control = list(reltol = 1e-12))$par)
}


# new local comment