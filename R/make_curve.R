## Function file for enzKinet2 package

#' Make Curve
#'
#' @author Haig Bishop
#' 07/09/2024
#'
#' Generates a curve for a model. The "curve" is equivalent to dense noiseless data.
#' @param model The model to generate the curve from ("MM", "CI", etc.)
#' @param params The parameter values for the model (e.g. Km, Vmax, Ksi)
#' @param x.min The minimum x value to cover.
#' @param x.max The maximum x value to cover.
#' @param z.values The z values to cover (discrete).
#' @param n.samples The resolution of the curve (~150 is usually enough).
#' @param space The distribution of the space to generate data.
#' @param conf.int Whether or not to generate upper and lower bound curves.
#' @return curve.df
#' 
#' @export

make_curve <- function(model, params, x.min, x.max, z.values = NULL, 
                       n.samples = 250, space = "linear", conf.int = FALSE) {
    
    
    # Generate noiseless data
    curve.df <- simulate_data(model, params, x.min, x.max, z.values = z.values, 
                              noise.level = 0, n.samples = n.samples, space = space)
    
    
    # If confidence intervals are requested, generate lower and upper bound curves ====
    if (conf.int) {
        # Check params has all necessary lower and upper bounds
        needed.bounding.params <- CONFIDENCE_INTERVAL_BOUNDING_PARAMS[[model]]
        missing_bounds <- needed.bounding.params[!needed.bounding.params %in% names(params)]
        if (length(missing_bounds) > 0) {
            stop(paste("Missing confidence interval values for:", paste(missing_bounds, collapse = ", ")))
        }
        
        # Initialize lists to store lower and upper bound parameters
        lb.params <- list()
        ub.params <- list()
        # Iterate over each parameter in the LOWER_BOUND_PARAMS dictionary
        for (param in names(LOWER_BOUND_PARAMS)) {
            # Sometimes there may be two keys lb and ub (e.g. Hill)
            lower_bound_keys <- LOWER_BOUND_PARAMS[[param]]
            upper_bound_keys <- UPPER_BOUND_PARAMS[[param]]
            
            # Check if the parameter's lower bound exists in the params list
            if (any(lower_bound_keys %in% names(params))) {
                # Get the corresponding value in params
                result.vec <- c()
                for (lb.key in lower_bound_keys) {
                    result.vec <- c(result.vec, params[[lb.key]])
                }
                lb.params[[param]] <- result.vec
            }
            
            # Check if the parameter's upper bound exists in the params list
            if (any(upper_bound_keys %in% names(params))) {
                # Get the corresponding value in params
                result.vec <- c()
                for (ub.key in upper_bound_keys) {
                    result.vec <- c(result.vec, params[[ub.key]])
                }
                ub.params[[param]] <- result.vec
            }
        }
        # Generate all combinations of parameter values
        lb.param_combinations <- expand.grid(lb.params)
        # Initialize a variable to store the minimum values across all simulations
        lb.curve.df <- NULL
        
        # Loop through each combination of parameters (Usually just one, but sometimes more - e.g. Hill)
        for (i in 1:nrow(lb.param_combinations)) {
            # Extract the parameters for this combination
            lb.params <- as.list(lb.param_combinations[i, ])
            names(lb.params) <- names(lb.param_combinations)
            lb.params <- lapply(lb.params, function(x) unclass(x))
            # Generate lower bound curve
            current.lb.curve.df <- simulate_data(model, lb.params, x.min, x.max, z.values = z.values, 
                                                 noise.level = 0, n.samples = n.samples, space = space)
            # If this is the first iteration, initialize lb.curve.df with the current current.lb.curve.df
            if (is.null(lb.curve.df)) {
                lb.curve.df <- current.lb.curve.df
            } else {
                # Compare the current lb.curve.df with the stored minimums and update lb.curve.df
                lb.curve.df <- pmin(lb.curve.df, current.lb.curve.df)
            }
        }
        
        # Generate all combinations of parameter values
        ub.param_combinations <- expand.grid(ub.params)
        # Initialize a variable to store the maximum values across all simulations
        ub.curve.df <- NULL
        
        # Loop through each combination of parameters (Usually just one, but sometimes more - e.g. Hill)
        for (i in 1:nrow(ub.param_combinations)) {
            # Extract the parameters for this combination
            ub.params <- as.list(ub.param_combinations[i, ])
            names(ub.params) <- names(ub.param_combinations)
            ub.params <- lapply(ub.params, function(x) unclass(x))
            
            # Generate upper bound curve
            current.ub.curve.df <- simulate_data(model, ub.params, x.min, x.max, z.values = z.values, 
                                                 noise.level = 0, n.samples = n.samples, space = space)
            # If this is the first iteration, initialize ub.curve.df with the current current.ub.curve.df
            if (is.null(ub.curve.df)) {
                ub.curve.df <- current.ub.curve.df
            } else {
                # Compare the current current.ub.curve.df with the stored maximums and update ub.curve.df
                ub.curve.df <- pmax(ub.curve.df, current.ub.curve.df)
            }
        }
        
        # Rename columns to reflect lower and upper bound curves
        names(lb.curve.df) <- paste0(names(lb.curve.df), ".lb")
        names(ub.curve.df) <- paste0(names(ub.curve.df), ".ub")
        # Combine original curve with lower and upper bound curves
        curve.df <- cbind(curve.df, lb.curve.df, ub.curve.df)
    }
    # ===================================================================
    
    
    return(curve.df)
}
