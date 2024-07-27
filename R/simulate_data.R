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
#' @param n_replicates The number of replicate data points generated.
#' @param noise_level The level of noise (equal to or greater than 0)
#' @param noise_type The kind of noise ("absolute" or "relative")
#' @param space The distribution of the space to generate data
#' @return synthetic.data
#' 
#' @export

simulate_data <- function(model, params, x.min, x.max, z.values = NULL, 
                          n_samples = 24, n_replicates = 1, 
                          noise_level = 0.05, noise_type = "relative", 
                          space = "linear") {
    
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
    # Check if x.min is less than x.max
    if (x.min >= x.max) {
        stop("x.min must be less than x.max.")
    }
    # Check if n_samples is a positive integer
    if (!is.numeric(n_samples) || n_samples <= 0 || floor(n_samples) != n_samples) {
        stop("n_samples must be a positive integer.")
    }
    # Check if n_replicates is a positive integer
    if (!is.numeric(n_replicates) || n_replicates <= 0 || floor(n_replicates) != n_replicates) {
        stop("n_replicates must be a positive integer.")
    }
    # Check if noise_level is numeric and equal to or greater than 0
    if (!is.numeric(noise_level) || noise_level < 0) {
        stop("noise_level must be a numeric and equal to or greater than 0.")
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
    if (space == "linear") {
        x.range <- pracma::linspace(x.min, x.max, n = n_samples)
    } else if (space == "exponential"){
        # Generate an exponentially spaced sequence from 0 to 1
        exp_seq <- (seq(0, 1, length.out = n_samples))^2
        # Normalize the sequence to the desired range
        x.range <- exp_seq * (x.max - x.min) + x.min
    } else if (space == "inverse_exponential"){
        # Generate an exponentially spaced sequence from 0 to 1
        exp_seq <- (seq(0, 1, length.out = n_samples))^3
        # Flip it
        #exp_seq <- (exp_seq - 1) * -1
        # Normalize the sequence to the desired range
        x.range <- exp_seq * (x.max - x.min) + x.min
    }
    
    # Get the model function
    model.function <- MODEL_FUNCTIONS[[model]]
    # Generate a perfect curve
    synthetic.data <- model.function(params, x.range, z.values)
    # ===================================
    
    # Replicate data =================
    # Duplicate the data n_replicates times
    synthetic.data <- do.call(rbind, replicate(n_replicates, synthetic.data, simplify = FALSE))
    # ===================================
    
    # Add noise ============================
    # If any noise
    if (noise_level > 0) {
        # Make a noisy normal distribution for each observation
        num_observations <- nrow(synthetic.data)
        noise <- rnorm(num_observations, mean = 0, sd = noise_level)
        
        # If noise_type is relative
        if (noise_type == "relative") {
            # Scale by the data
            noise <- synthetic.data[[dependent.var]] * noise
        }
        
        # Add noise to data
        synthetic.data[[dependent.var]] <- synthetic.data[[dependent.var]] + noise
        
        # Get the valid domain of the dependent variable
        dependent.var.domain <- MODEL_DEPENDENT_VAR_DOMAINS[[model]]
        minimum = dependent.var.domain[1]
        maximum = dependent.var.domain[2]
        # Ensure the data is within the range
        original_data <- synthetic.data[[dependent.var]]
        clipped_data <- pmin(synthetic.data[[dependent.var]], maximum)
        synthetic.data[[dependent.var]] <- pmax(clipped_data, minimum)
        
        # Warn user if any data was clipped
        if (any(synthetic.data[[dependent.var]] != original_data)) {
            warning("Some data values were clipped to fit within the range [", minimum, ", ", maximum, "].")
        }
    }
    # ===============================
    
    
    return(synthetic.data)
}
