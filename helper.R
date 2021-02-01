# shifts positions of a vector by x steps
shifter <- function(n = 1, x) {
	if (n == 0) x else c(tail(x, -n), head(x, n))
}

# maps action id to state id
get_state_id <- function(action_ids, m) {   # needs to be adjusted for n > 3!!!
	(action_ids[1] - 1) * m + action_ids[2]
}