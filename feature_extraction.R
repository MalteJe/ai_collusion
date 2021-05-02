# A - Polynomial ----------------------------------------------------------

# set_up_poly sets up the polynomial feature extraction (utilized in poly_tiling and poly_separated)
set_up_poly <- function(specifications,    # list with named element: degree
								vars               # number of variables to enter the feature vector, in this study: 2 or 3 (if action enters feature vector)
) {
	
	# retrieve all possible combination of exponents given the specified degree and number of variables (every row is one feature)
	expon <- expand.grid(rep(list(0:specifications$degree), vars)) %>%
		filter(between(rowSums(.), 1, specifications$degree))
	
	# arrange in order and stores as matrix (for computational efficiency)
	exponents <- expon %>%
		arrange(get(rev(names(expon)))) %>%
		as.matrix()
	
	return(list(
		exponents = exponents,   # matrix of exponents
		le = nrow(exponents),  # number of features
		x_default = numeric(nrow(exponents)))  
		)
}


# get_x_poly extracts the features from a state-action combination given a specification from 'set_up_poly'
get_x_poly  <- function(state_set,        # state set (in this study: length of 2)
								action,           # considered action, may be NULL
								feature_specs     # specifications obtained from set_up_poly
								) {
	
	# concatenate state and action
	s_a_t <- c(state_set, action)
	
	# take power from exponent matrix to state-action combination (piecewise)
	intermediate <- matrix(s_a_t, nrow = feature_specs$le, ncol = length(s_a_t), byrow = T)^feature_specs$exponents
	
	# rowprowducts (every row is reduced to 1 feature)
	return(apply(X = intermediate, MARGIN = 1, FUN = prod))
}




# fs_poly <- set_up_poly(list(degree = 6), vars = 3)
# get_x_poly(c(1,2), 1.8, fs_poly)





# A2 - Separate Polynomials -----------------------------------------------

# set_up_poly_separate sets up the separete polynomial feature extraction with a distinct polynomial for every feasible action
set_up_poly_separate <- function(specifications,    # list with named element: degree_sep
											available_prices   # vector of feasible prices
) {
	
	# calculate exponents matrix, since the action doesn't enter the polynomial, vars = 2
	exponents <- set_up_poly(list(degree = specifications$degree_sep), vars = 2)$exponents
	
	# get the number of features PER polynomial
	m <- length(available_prices)
	le <- choose(specifications$degree_sep + 2L, 2L) - 1L
	
	# obtain length of feature vector and fill with 0 by default
	x_default <- rep(0, m * le)
	
	
	
	return(list(exponents = exponents,          # matrix of exponents
					x_default = x_default,          # empty feature vector
					le = as.integer(le),            # length PER polynomial
					thresholds = available_prices   # feasible actions
	))
}


# get_x_poly_separate extracts the features from a state-action combination given a specification from 'set_up_poly_separate'
get_x_poly_separate <- function(state_set,        # state set (in this study: length of 2)
										  action,           # considered action
										  feature_specs     # specifications obtained from set_up_poly_separate
) {
	# extract polynomial features from state-action combination
	raw <- get_x_poly(state_set = state_set, action = NULL, feature_specs = feature_specs)
	
	
	# identify the action id and find leftmost (-1) representative position in feature vector
	action_id <- findInterval(x = action, vec = feature_specs$thresholds, rightmost.closed = FALSE, all.inside = FALSE) - 1L
	activated_demarcation <- action_id * feature_specs$le
	
	# replace default vector with polynomial at appropriate places
	feature_specs$x_default[(activated_demarcation + 1L):(activated_demarcation + feature_specs$le)] <- raw
	
	return(feature_specs$x_default)
}


available_prices <- seq(from = 1, to = 2.4, by = 0.1)
fs_separate <- set_up_poly_separate(list(degree_sep = 4), available_prices = available_prices)
get_x_poly_separate(state_set = c(1.4, 2), action = 1.1, feature_specs = fs_separate)


# B - Tile Coding -------------------------------------------------------------


# B - 1: Set Up ------------------------------------------------------------------

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



# B - 2: Calculate features ---------------------------------------------------


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
	feature_specs$x_default[x_positions] <- TRUE
	
	return(feature_specs$x_default)
}


# tiling_specs <- set_up_tilings(specifications = list(n_tilings = 6L, n_tiles = 10L),
# 				 min_price = 1, max_price = 2.4, vars = 3)
# tiling_specs2 <- set_up_tilings(specifications = list(n_tilings = 6L, n_tiles = 10L),
# 										  min_price = 1, max_price = 2.4, vars = 2)
# 
# get_x_tiling(c(1, 1.5), 2, tiling_specs)
# get_x_tiling(c(2, 2), NULL, tiling_specs2)



# C: Polynomial Tiling -------------------------------------------------

# set_up_poly_tilings sets up the polynomial tiling feature extraction
set_up_poly_tilings <- function(specifications,  # list with named elements regarding number of tiles and tilings
										  min_price,       # minimum price
										  max_price,       # maximum price
										  vars             # number of variables to enter the feature vector
) {

	# calculate exponents matrix
	exponents <- set_up_poly(list(degree_ = specifications$degree_poly_tiling), vars = vars)$exponents
	
	# calculate length of single polynomials
	le <- choose(specifications$degree_poly_tiling + vars, vars) - 1L
	
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
	
	# create default vector with all tiles deactivated
	x_default<- rep(0, specifications$poly_n_tilings * specifications$poly_n_tiles^vars * le)
	
	# return specifications as list
	return(list(
		exponents = exponents,
		le = as.integer(le),
		n_tilings = specifications$poly_n_tilings,
		tilings = tilings,
		coordinate_mapping = coordinate_mapping,
		tiling_mapping = tiling_mapping,
		x_default = x_default))
}

# get_x_positions extends the rightmost position of the activated features to a length compatiable with the number of features from simple polynomial feature extraction
get_x_positions <- function(right_boundary, length_poly) {
	(right_boundary - length_poly + 1):right_boundary
}

# get_x_poly_tliings extracts the features from a state-action combination given a specification from 'set_up_poly_tilings'
get_x_poly_tilings <- function(state_set,     # state set (i.e. vector of length 2)
										 action,        # action
										 feature_specs  # tiling specifications obtained from 'set_up_tilings'
) {  
	
	# get raw polynomial
	raw <- get_x_poly(state_set = state_set, action = action, feature_specs = feature_specs)
	
	# retrieve the active tiles of every tiling
	active_tiles <- vapply(X = feature_specs$tilings,
								  FUN = get_active_tile,
								  FUN.VALUE = integer(1L),
								  state_action = c(state_set, action),
								  coordinate_mapping = feature_specs$coordinate_mapping)
	
	# map positions from within a tiling to the positions in the feature vector
	right_boundary <- (active_tiles + feature_specs$tiling_mapping) * feature_specs$le
	
	x_positions <- vapply(X = right_boundary,
								 FUN = get_x_positions,
								 FUN.VALUE = numeric(feature_specs$le),
								 length_poly = feature_specs$le)
	
	
	
	# map positions from within
	
	# return feacture vector as logical with activated tiles identified with TRUE
	feature_specs$x_default[x_positions] <- rep(raw, feature_specs$n_tilings)
	
	return(feature_specs$x_default)
}


# input <- list(degree_poly_tiling = 3,
# 				  poly_n_tilings = 2,
# 				  poly_n_tiles = 3)
# s_t <- c(2.4, 1)
# a_t <- 1.8
# fs_poly_tilings <- set_up_poly_tilings(input, min_price = 1, max_price = 2.4, vars = 3)
# (res <- get_x_poly_tilings(s_t, a_t, fs_poly_tilings))


# 
# 
# microbenchmark(
# 	get_x_poly(s_t, a_t, fs_poly),
# 	get_x_poly_separate(s_t, a_t, fs_separate),
# 	get_x_tiling(s_t, a_t, tiling_specs),
# 	get_x_tiling(s_t, NULL, tiling_specs2),
# 	get_x_poly_tilings(s_t, a_t, fs_poly_tilings)
# )

