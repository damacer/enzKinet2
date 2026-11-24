## Function file for enzKinet2 package

#' Make a model curve
#'
#' Computes a model's curve: dense, noise-free values of the dependent variable
#' across a range of the independent variable, ready to plot with
#' [make_plot()]. It calls [simulate_data()] with no noise. With
#' `conf.int = TRUE` it also computes a parameter uncertainty envelope from the
#' confidence bounds that [fit_model()] returns.
#'
#' @details
#' **The envelope.** The lower envelope curve uses, for each parameter,
#' whichever of its confidence bounds makes the curve lower (e.g. the upper
#' bound of `Km` and the lower bound of `Vmax`), and the upper curve uses the
#' other bound. For a Hill coefficient `n`, both bounds are tried and the
#' lowest and highest curves kept. Bounds of 0 or less are raised to 1e-10. The
#' envelope shows how far the curve moves when each parameter is at the edge
#' of its own confidence interval. It is not a confidence band for the curve,
#' and it can be wider than one, because it ignores the correlation between
#' the parameters.
#'
#' @inheritParams simulate_data
#' @param params A named list of parameter values, as for [simulate_data()].
#'   With `conf.int = TRUE` it must also hold each parameter's bounds, named
#'   `<parameter>.lb` and `<parameter>.ub` (e.g. `Km.lb`), as in
#'   `fit_model()$fitted.params`.
#' @param n.samples The number of points along the curve (default 250, smooth
#'   enough for most plots).
#' @param space How the points along the curve are spaced: `"linear"`
#'   (default), `"exponential"` (denser at low x), `"inverse.exponential"`
#'   (denser at high x), `"dilution.series"` or `"custom"`, as described in
#'   [simulate_data()].
#' @param conf.int If `TRUE`, also compute the lower and upper envelope curves
#'   (see Details). Default `FALSE`.
#' @return A data frame with the same columns as [simulate_data()]. With
#'   `conf.int = TRUE`, each column is repeated with the suffixes `.lb` and
#'   `.ub` (e.g. `A.lb`, `V.lb`, `A.ub`, `V.ub`) for the lower and upper
#'   envelope curves.
#' @seealso [make_plot()] to plot the curve, [fit_model()] for fitted
#'   parameters and their bounds.
#' @author Haig Bishop
#' @examples
#' curve <- make_curve("MM", list(Km = 0.5, Vmax = 2), x.min = 0, x.max = 5)
#' head(curve)
#' make_plot("MM", curve.df = curve)
#'
#' # One curve per inhibitor concentration
#' make_plot("CI", curve.df = make_curve("CI", list(Km = 0.5, Vmax = 2, Ki = 1),
#'                                       x.min = 0, x.max = 5, z.values = c(0, 1, 2)))
#'
#' # The envelope from a fit's confidence intervals
#' set.seed(1)
#' mm <- simulate_data("MM", list(Km = 0.5, Vmax = 2), x.min = 0.1, x.max = 5,
#'                     n.samples = 8, n.replicates = 3)
#' fit <- fit_model("MM", mm, start.params = list(Km = 1, Vmax = 1),
#'                  get.conf.int = TRUE)
#' envelope <- make_curve("MM", fit$fitted.params, x.min = 0, x.max = 5,
#'                        conf.int = TRUE)
#' make_plot("MM", data.df = mm, curve.df = envelope, conf.int = TRUE)
#' @export

make_curve <- function(model, params, x.min, x.max, z.values = NULL, 
                       n.samples = 250, space = "linear", conf.int = FALSE,
                       dilution.factor = NULL, x.range = NULL) {
    
    
    # Generate noiseless data
    curve.df <- simulate_data(model, params, x.min, x.max, z.values = z.values, 
                              noise.level = 0, n.samples = n.samples, space = space,
                              dilution.factor = dilution.factor, x.range = x.range)
    
    
    # If a confidence envelope is requested, generate envelope curves from parameter CI bounds ====
    if (conf.int) {
        # Check params has all necessary lower and upper bounds
        needed.bounding.params <- CONFIDENCE_INTERVAL_BOUNDING_PARAMS[[model]]
        missing_bounds <- needed.bounding.params[!needed.bounding.params %in% names(params)]
        if (length(missing_bounds) > 0) {
            stop(paste("Missing parameter confidence interval bounds for:", paste(missing_bounds, collapse = ", ")))
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
        # Model functions require positive parameters. Clamp non-positive CI bounds.
        # A near-zero Km.lb makes the upper MM envelope sit at Vmax.
        lb.params <- lapply(lb.params, function(x) pmax(x, 1e-10))
        ub.params <- lapply(ub.params, function(x) pmax(x, 1e-10))
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
                                                 noise.level = 0, n.samples = n.samples, space = space,
                                                 dilution.factor = dilution.factor, x.range = x.range)
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
                                                 noise.level = 0, n.samples = n.samples, space = space,
                                                 dilution.factor = dilution.factor, x.range = x.range)
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
