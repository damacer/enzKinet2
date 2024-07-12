## Function file for enzKinet2 package

#' Simulate Data
#'
#' @author Haig Bishop
#' 28/06/2024
#'
#' Generates synthetic data for an enzyme kinetics model.
#' @param model The chosen model ("MM", "MMSI", etc.)
#' @param params The parameters for the model (Including Km, Vmax, Ksi, etc.)
#' @param x.min Defines the range of x values to cover.
#' @param x.max Defines the range of x values to cover.
#' @param z.values Defines the z values to cover (NULL if not used)
#' @param n_samples The number of data points generated.
#' @param noise_level The level of noise (between 0 and 1, inclusive)
#' @param noise_type The kind of noise ("absolute" or "relative")
#' @return synthetic.data
#' 
#' @export

simulate_data <- function(model, params, x.min, x.max, z.values = NULL, n_samples = 24, noise_level = 0.05, noise_type = "relative") {
    
    # Error Handling ================
    # Check if model is valid
    if (!model %in% VALID_MODELS) {
        stop("Invalid model. Please choose a valid model such as 'MM'.")
    }
    # Check if params is a list
    if (!is.list(params)) {
        stop("params must be a list.")
    }
    # Check if x.min and x.max are numeric
    if (!is.numeric(x.min) || !is.numeric(x.max)) {
        stop("x.min and x.max must be numeric values.")
    }
    # Check if x.min is positive
    if (x.min > 0) {
        stop("x.min must be a positive value.")
    }
    # Check if x.min is less than x.max
    if (x.min >= x.max) {
        stop("x.min must be less than x.max.")
    }
    # Check if n_samples is a positive integer
    if (!is.numeric(n_samples) || n_samples <= 0 || floor(n_samples) != n_samples) {
        stop("n_samples must be a positive integer.")
    }
    # Check if noise_level is numeric and between 0 and 1
    if (!is.numeric(noise_level) || noise_level < 0 || noise_level > 1) {
        stop("noise_level must be a numeric value between 0 and 1 (inclusive).")
    }
    # Check if noise_type is valid
    if (!noise_type %in% c("absolute", "relative")) {
        stop("noise_type must be either 'absolute' or 'relative'.")
    }
    # ===============================
    
    
    # Model-specific error handling =======
    # Grab model-specific values
    param.names <- MODEL_PARAMETERS[[model]]
    full.model.name <- PLOT_TITLES[[model]]
    model.params.string <- MODEL_PARAMETER_STRINGS[[model]]
    dependent.var <- MODEL_VARIABLES[[model]][2]
    num.independent.vars <- length(MODEL_VARIABLES[[model]]) - 1
    
    # Check if required parameters are present
    if (!all(param.names %in% names(params))) {
        stop(paste("For the", full.model.name, "model, params must include", model.params.string, "."))
    }
    
    # Check if the parameters are numeric and greater than 0
    for (param in param.names) {
        if (!is.numeric(params[[param]])) {
            stop(paste(param, "must be a numeric value."))
        }
        if (params[[param]] <= 0) {
            stop(paste(param, "must be greater than 0."))
        }
    }
    # If the model has 2 independent variables, check z.values
    if (num.independent.vars == 2) {
        if (is.null(z.values) || length(z.values) < 1) {
            stop("For models with two independent variables, z.values must be provided and contain at least one value.")
        }
        if (any(z.values < 0)) {
            stop("All values in z.values must be greater than or equal to 0.")
        }
    }
    # ===================================
    
    
    # Generate the perfect curve ================
    # Define values of x
    x.range <- pracma::linspace(x.min, x.max, n = n_samples)
    # Get the model function
    model.function <- MODEL_FUNCTIONS[[model]]
    # Generate a perfect curve
    synthetic.data <- model.function(params, x.range, z.values)
    # ===================================
    
    
    # Add noise ============================
    # If any noise
    if (noise_level > 0) {
        # Make a noisy normal distribution
        noise <- rnorm(n_samples, mean = 0, sd = 1) * noise_level
        
        # If noise_type is relative
        if (noise_type == "relative") {
            # Scale by the data
            noise <- synthetic.data[[dependent.var]] * noise
        }
        
        # Add noise to data
        synthetic.data[[dependent.var]] <- synthetic.data[[dependent.var]] + noise
    }
    # ===============================
    
    
    return(synthetic.data)
}
