# A - Polynomial ----------------------------------------------------------

set_up_poly <- function(specifications,    # list with named element: polynomial degree
								vars               # number of variables to enter the feature vector
) {
	
	# browser()
	
	expon <- expand.grid(rep(list(0:specifications$degree), vars)) %>%
		filter(between(rowSums(.), 1, specifications$degree))
	
	exponents <- expon %>%
		arrange(get(rev(names(expon)))) %>%
		as.matrix()
	# exponents <- expand_grid(x = 0:specifications$degree, y =  0:specifications$degree, z = 0:specifications$degree) %>%
	# 	filter(between(x+y+z, 1, specifications$degree)) %>%
	# 	arrange(z,y,x) %>%
	# 	as.matrix()
	
	# exponents[exponents == 0] <- NA
	return(list(exponents = exponents, nrow = nrow(exponents)))
}


get_x_poly  <- function(state_set, action, feature_specs) {
	s_a_t <- c(state_set, action)
	intermediate <- matrix(s_a_t, nrow = feature_specs$nrow, ncol = 3, byrow = T)^feature_specs$exponents
	
	return(rowprods(intermediate))
}

# fs_poly <- set_up_poly(list(degree = 6), vars = 3)
# get_x_poly(c(1,2), 1.8, fs_poly)




# A2 - Polynomial into normalization

set_up_poly_normalized <- function(specifications,    # list with named element: polynomial degree,
											  min_price,         # minimum price
											  max_price,         # maximum price
											  vars               # number of variables to enter the feature vector
) {
	exponents <- set_up_poly(specifications, vars = vars)$exponents
	
	minimums <- apply(X = exponents, MARGIN = 1, FUN = function(x) prod(min_price^x, na.rm = TRUE))
	maximums <- apply(X = exponents, MARGIN = 1, FUN = function(x) prod(max_price^x, na.rm = TRUE))
	differences <- maximums - minimums
	
	
	list(exponents = exponents, mins = minimums, maxs = maximums, diffs = differences)
}

get_x_poly_normalized <- function(state_set, action, feature_specs) {
	raw <- apply(feature_specs$exponents, 1, function(x) prod((c(state_set, action))^x, na.rm = TRUE))
	
	2 * (raw - feature_specs$mins)/(feature_specs$diffs) - 1
}


# fs_normalized <- set_up_poly_normalized(list(degree = 5), min_price = 1, max_price = 2.4, vars = 3)
# 
# get_x_poly_normalized(c(1,1.5), 2, fs_normalized)



# A3 - Separate Polynomials -----------------------------------------------


set_up_poly_separate <- function(specifications,    # list with named element: polynomial degree
											available_prices,  # vector of feasible prices
											vars               # number of variables to enter the feature vector
) {
	
	# calculate exponents matrix
	exponents <- set_up_poly(list(degree = specifications$degree_sep), vars = vars)$exponents

	m <- length(available_prices)
	le <- choose(specifications$degree_sep + 2, 2) - 1
	
	x_default <- rep(0, m * le)
	
	
	
	return(list(exponents = exponents, x_default = x_default, le = le, thresholds = c(available_prices)))
}

get_x_poly_separate <- function(state_set, action, feature_specs) {
	
	raw <- apply(feature_specs$exponents, 1, function(x) prod((c(state_set))^x, na.rm = TRUE))
	
	action_id <- findInterval(x = action, vec = feature_specs$thresholds, rightmost.closed = FALSE, all.inside = FALSE) - 1L
	activated_demarcation <- action_id * feature_specs$le
	
	replace(x = feature_specs$x_default,
			  list = (activated_demarcation + 1):(activated_demarcation + feature_specs$le),
			  values = raw)
}


# available_prices <- seq(from = 1, to = 2.4, by = 0.1)
# fs_separate <- set_up_poly_separate(list(degree_sep = 4), available_prices = available_prices, vars = 2)
# get_x_poly_separate(state_set = c(1.4, 1.8), action = 1, feature_specs = fs_separate)




# B - Splines -------------------------------------------------------------

# B - 1: Set Up ------------------------------------------------------------------

# set_up_splines prepares inner knots  that will be continuously used through the loop in accordance with the inputs

set_up_splines <- function(specifications,    # list with named elements: degree and number of (inner) knots
									min_price,         # minimum price
									max_price,         # maximum price
									rounding_precision,# controls number of digits after comma
									vars               # number of variables to enter the feature vector
									) {
	
	# space out knots evenly within price range and remove outer knots
	knots_linear <- seq(from = min_price, to = max_price, length.out = specifications$n_knots + 2) %>%
		round(rounding_precision)
	inner_knots_linear <- knots_linear[-c(1, length(knots_linear))]
	
	# knots of interactions are squared (--> not evenly spaced out)
	inner_knots_interaction <- inner_knots_linear^2 %>%
		round(rounding_precision)
	
	# return list of specified degree and inner knots iterated 
	return(list(
		degree = specifications$splines_degree,
		inner_knots = c(rep(list(inner_knots_linear), vars), rep(list(inner_knots_interaction),((vars-1)^2 + (vars-1))/2))
	))
}

# B - 2: Calculate features ---------------------------------------------------

# get_spline calculates a piecewise polynomial of specified degree for a single input, restrictions are fixed as 'degree - 1', no intercept

get_spline <- function(x, degree = 3, knots) {
	h1 <- x^(1:degree)
	h2 <- ifelse(x > knots, (x - knots)^degree, 0)
	return(c(h1, h2))
}


# get_x_splines returns a feature vector of piecewise polynomials of (i) state, (ii) action and (iii) simple interaction information

get_x_splines <- function(state_set, action, feature_specs) {
	
	components <- c(state_set, action, prod(state_set), state_set * action)
	
	map2(.x = components,
		  .y = feature_specs$inner_knots,
		 .f = get_spline,
		 degree = feature_specs$degree) %>%
		unlist()
}


get_x_splines2 <- function(state_set, action, feature_specs) {
	
	components <- c(state_set, action)
	map(.x = components,
		 .f = get_spline,
		 degree = feature_specs$degree,
		 knots = feature_specs$inner_knots[[1]]) %>%
		expand.grid() %>%
		apply(MARGIN = 1, FUN = prod)
}


# splines_fs <- set_up_splines(specifications = list(splines_degree = 4, n_knots = 3), min_price = 1, max_price = 2.4, rounding_precision = 8L, vars = 3)
# 
# get_x_splines(c(1.5,1.9), action = 2.4, splines_fs)
# get_x_splines2(c(1.5,1.9), action = 2.4, splines_fs)

# microbenchmark(get_x_splines(c(1.3, 1.7), 3, feature_specs = fs),
# 					get_x_splines2(c(1.3, 1.7), 3, feature_specs = fs))

# C - Tile Coding -------------------------------------------------------------


# C - 1: Set Up ------------------------------------------------------------------

# set_up_tilings prepares tilings and other information that will be continuously used through the loop in accordance with the inputs

set_up_tilings <- function(specifications,  # list with named elements regarding number of tiles and tilings
									min_price,       # minimum price
									max_price,       # maximum price
									vars               # number of variables to enter the feature vector
									) {
	
	# convert to integers to ensure efficient calculations
	if (!is.integer(specifications$n_tilings)) {specifications$n_tilings <- as.integer(specifications$n_tilings)}
	if (!is.integer(specifications$n_tiles))   {specifications$n_tiles   <- as.integer(specifications$n_tiles)}
	
	# lay out default tile cutoffs
	main_tiling <- seq(from = min_price, to = max_price, length.out = specifications$n_tiles) - 1*10^-6
	
	# determine offsets for other tilings
	(max_offset <- main_tiling[2] - main_tiling[1] - 2*10^-6)   # the final subtraction ensures that the minimal price is always in the first tile available
	offsets <- seq(from = 0, to = max_offset, length.out = specifications$n_tilings)
	
	# lay out all tilings
	tilings <- map(.x = offsets,
						.f = ~main_tiling - .x)
	
	
	# create vector for mapping coordinates to position in feature vector (within tiling)
	coordinate_mapping <- specifications$n_tiles^((vars-1):0) %>% as.integer()
	
	# create vector for mapping position within tiling to position in feature vector
	tiling_mapping <- as.integer(specifications$n_tiles^vars) * 0L:(specifications$n_tilings - 1L)
	
	# create logical with all tiles deactivated
	x_default<- rep(FALSE, specifications$n_tilings * specifications$n_tiles^vars)
	
	# return specifications as list
	return(list(
		tilings = tilings,
		coordinate_mapping = coordinate_mapping,
		tiling_mapping = tiling_mapping,
		x_default = x_default))
}



# C - 2: Calculate features ---------------------------------------------------


# get_active_tile retrieves the position of the active tile within a single tiling

get_active_tile <- function(tiling,            # vector with tiling thresholds
									 state_action,      # vector with state and action space
									 coordinate_mapping # vector to map coordinates to position
									 ) {
	
	# find appropriate thresholds for every state and action
	coordinates <- findInterval(x = state_action, vec = tiling, rightmost.closed = FALSE, all.inside = FALSE) - 1L
	
	# map the coordinates to position in the particular tiling
	sum(coordinates * coordinate_mapping) + 1L  # return active tile position
}



# get_x_tiling maps the state-action combination to the feature vector in accordance with the tiling specifications

get_x_tiling <- function(state_set,     # state set (i.e. vector of length 2)
								 action,        # action
								 feature_specs  # tiling specifications obtained from 'set_up_tilings'
								 ) {  
	
	# concatenate state and action space
	s_a_t <- c(state_set, action)
	
	# retrieve the active tiles of every tiling
	active_tiles <- map_int(.x = feature_specs$tilings,
									.f = get_active_tile,
									state_action = s_a_t,
									coordinate_mapping = feature_specs$coordinate_mapping)
	
	# map positions from within a tiling to the positions in the feature vector
	x_positions <- active_tiles + feature_specs$tiling_mapping
	
	# return feacture vector as logical with activated tiles identified with TRUE
	return(replace(x = feature_specs$x_default, list = x_positions, values = TRUE))
}



# tiling_specs <- set_up_tilings(specifications = list(n_tilings = 6L, n_tiles = 10L),
# 				 min_price = 1, max_price = 2.4, vars = 3)
# 
# tiling_specs2 <- set_up_tilings(specifications = list(n_tilings = 6L, n_tiles = 10L),
# 										  min_price = 1, max_price = 2.4, vars = 2)
# 
# get_x_tiling(c(1, 1.5), 2, tiling_specs)
# get_x_tiling(c(2, 2), NULL, tiling_specs2)


# D: Polynomial Tiling -------------------------------------------------

set_up_poly_tilings <- function(specifications,  # list with named elements regarding number of tiles and tilings
										  min_price,       # minimum price
										  max_price,       # maximum price
										  vars             # number of variables to enter the feature vector
) {
	
	# calculate exponents matrix
	exponents <- set_up_poly(list(degree_ = specifications$degree_poly_tiling), vars = vars)$exponents
	
	# calculate length of single polynomials
	le <- choose(specifications$degree_poly_tiling + vars, vars) - 1
	
	
	# convert to integers to ensure efficient calculations
	if (!is.integer(specifications$poly_n_tilings)) {specifications$poly_n_tilings <- as.integer(specifications$poly_n_tilings)}
	if (!is.integer(specifications$poly_n_tiles))   {specifications$poly_n_tiles   <- as.integer(specifications$poly_n_tiles)}
	
	# lay out default tile cutoffs
	main_tiling <- seq(from = min_price, to = max_price, length.out = specifications$poly_n_tiles) - 1*10^-6
	
	# determine offsets for other tilings
	max_offset <- main_tiling[2] - main_tiling[1] - 2*10^-6   # the final subtraction ensures that the minimal price is always in the first tile available
	offsets <- seq(from = 0, to = max_offset, length.out = specifications$poly_n_tilings)
	
	# lay out all tilings
	tilings <- map(.x = offsets,
						.f = ~main_tiling - .x)
	
	
	# create vector for mapping coordinates to position in feature vector (within tiling)
	coordinate_mapping <- specifications$poly_n_tiles^((vars-1):0) %>% as.integer()
	
	# create vector for mapping position within tiling to position in feature vector
	tiling_mapping <- as.integer(specifications$poly_n_tiles^vars) * 0L:(specifications$poly_n_tilings - 1L)
	
	# create logical with all tiles deactivated
	x_default<- rep(0, specifications$poly_n_tilings * specifications$poly_n_tiles^vars * le)
	
	# return specifications as list
	return(list(
		exponents = exponents,
		le = le,
		n_tilings = specifications$poly_n_tilings,
		tilings = tilings,
		coordinate_mapping = coordinate_mapping,
		tiling_mapping = tiling_mapping,
		x_default = x_default))
}




get_x_positions <- function(right_boundary, length_poly) {
	(right_boundary - length_poly + 1):right_boundary
}


get_x_poly_tilings <- function(state_set,     # state set (i.e. vector of length 2)
										 action,        # action
										 feature_specs  # tiling specifications obtained from 'set_up_tilings'
) {  
	
	
	# concatenate state and action space
	s_a_t <- c(state_set, action)
	
	# get raw polynomial
	raw <- apply(feature_specs$exponents, 1, function(x) prod((s_a_t)^x, na.rm = TRUE))
	
	# retrieve the active tiles of every tiling
	active_tiles <- map_int(.x = feature_specs$tilings,
									.f = get_active_tile,
									state_action = s_a_t,
									coordinate_mapping = feature_specs$coordinate_mapping)
	
	# map positions from within a tiling to the positions in the feature vector
	right_boundary <- (active_tiles + feature_specs$tiling_mapping) * feature_specs$le
	
	x_positions <- map(.x = right_boundary,
							 .f = get_x_positions,
							 length_poly = feature_specs$le) %>%
		purrr::flatten_int()
	
	# map positions from within
	
	# return feacture vector as logical with activated tiles identified with TRUE
	return(replace(x = feature_specs$x_default, list = x_positions, values = rep(raw, feature_specs$n_tilings)))
}

input <- list(degree_poly_tiling = 3,
				  poly_n_tilings = 2,
				  poly_n_tiles = 3)

fs_poly_tilings <- set_up_poly_tilings(input, min_price = 1, max_price = 2.4, vars = 2)
s_t <- c(2.4, 1)
a_t <- NULL

(res <- get_x_poly_tilings(s_t, a_t, fs_poly_tilings))
# 
# 
# microbenchmark(
# 	get_x_poly(s_t, a_t, fs_poly),
# 	get_x_poly_normalized(s_t, a_t, fs_normalized),
# 	get_x_poly_separate(s_t, a_t, fs_separate),
# 	get_x_splines(s_t, a_t, splines_fs),
# 	get_x_splines2(s_t, a_t, splines_fs),
# 	get_x_tiling(s_t, a_t, tiling_specs),
# 	get_x_tiling(s_t, NULL, tiling_specs2),
# 	get_x_poly_tilings(s_t, a_t, fs_poly_tilings)
# )



























# library(microbenchmark)
# 
# s_t <- c(2.4, 2.4)
# a_t <- 2.4
# 
# microbenchmark(
# 	get_x(state_set = s_t, action = a_t, degree = 3),
# 	get_x_tiling(state_set = s_t, action = a_t, feature_specs = tiling_specs)
# )







