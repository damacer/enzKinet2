## Function file for enzKinet2 package

#' Simulate Data
#'
#' @author Haig Bishop
#' 07/09/2024
#'
#' Generates synthetic data for a model.
#' @param model The model to generate the data from ("MM", "CI", etc.)
#' @param params The parameter values for the model (e.g. Km, Vmax, Ksi).
#' @param x.min The minimum x value to cover.
#' @param x.max The maximum x value to cover.
#' @param z.values The z values to cover (discrete).
#' @param n.samples The number of data points to generate (unique x values).
#' @param n.replicates The number of replicates of data points generated.
#' @param noise.level The level of noise (non-zero number).
#' @param noise.type The kind of noise ("absolute" or "relative").
#' @param space The distribution of the space to generate data.
#' @param dilution.factor Dilution factor if space == "dilution.series".
#' @param x.range If space is 'custom' these are the x values.
#' @param distribution The distribution to use for noise generation ("norm" or "truncated.norm").
#' @return synthetic.data
#' 
#' @export

simulate_data <- function(model, params, x.min, x.max, z.values = NULL, 
                          n.samples = 24, n.replicates = 1, 
                          noise.level = 0.05, noise.type = "relative", 
                          space = "linear", dilution.factor = NULL, x.range = NULL,
                          distribution = "norm") {
    
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
    # Check if n.samples is a positive integer
    if (!is.numeric(n.samples) || n.samples <= 0 || floor(n.samples) != n.samples) {
        stop("n.samples must be a positive integer.")
    }
    # Check if n.replicates is a positive integer
    if (!is.numeric(n.replicates) || n.replicates <= 0 || floor(n.replicates) != n.replicates) {
        stop("n.replicates must be a positive integer.")
    }
    # Check if noise.level is numeric and equal to or greater than 0
    if (!is.numeric(noise.level) || noise.level < 0) {
        stop("noise.level must be a non-negative numeric value.")
    }
    # Check if noise.type is valid
    if (!noise.type %in% c("absolute", "relative")) {
        stop("noise.type must be either 'absolute' or 'relative'.")
    }
    # Check if distribution is valid
    if (!distribution %in% c("norm", "truncated.norm")) {
        stop("Invalid distribution. Please choose 'norm' or 'truncated.norm'.")
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
        x.range <- pracma::linspace(x.min, x.max, n = n.samples)
    } else if (space == "exponential"){
        # Generate an exponentially spaced sequence from 0 to 1
        exp_seq <- (seq(0, 1, length.out = n.samples))^2
        # Normalize the sequence to the desired range
        x.range <- exp_seq * (x.max - x.min) + x.min
    } else if (space == "inverse.exponential"){
        # Generate an exponentially spaced sequence from 0 to 1
        exp_seq <- (seq(0, 1, length.out = n.samples))^3
        # Flip it
        #exp_seq <- (exp_seq - 1) * -1
        # Normalize the sequence to the desired range
        x.range <- exp_seq * (x.max - x.min) + x.min
    } else if (space == "dilution.series"){
        # Ensure we have a valid dilution factor
        if (is.null(dilution.factor) || !is.numeric(dilution.factor) || dilution.factor <= 1) {
            stop("A valid dilution.factor greater than 1 must be provided for dilution.series.")
        }
        # Generate x values using dilution factor
        x.range <- numeric(0)
        for (i in 1:n.samples) {
            value <- x.max / (dilution.factor ** (i - 1))
            # If not past minimum
            if (value >= x.min) {
                x.range <- c(x.range, value)
            } else {
                # Past minimum
                break
            }
        }
    } else if (space == "custom"){
        # Just ensure x.range is valid
        if (is.null(x.range) || !is.numeric(x.range) || length(x.range) < 2) {
            stop("For custom space, x.range must be a numeric vector with at least two values.")
        }
        if (any(x.range < x.min) || any(x.range > x.max)) {
            stop("All values in x.range must be within the bounds of x.min and x.max.")
        }
    } else {
        stop("Invalid space.")
    }
    
    # Get the model function
    model.function <- MODEL_FUNCTIONS[[model]]
    # Generate a perfect curve
    synthetic.data <- model.function(params, x.range, z.values)
    # ===================================
    
    # Replicate data =================
    # Duplicate the data n.replicates times
    if (n.replicates > 1) {
        synthetic.data <- do.call(rbind, replicate(n.replicates, synthetic.data, simplify = FALSE))
    }
    # ===================================
    
    # Add noise ============================
    # If any noise
    if (noise.level > 0) {
        # Make a noisy normal distribution for each observation
        num_observations <- nrow(synthetic.data)
        
        # Get the valid domain of the dependent variable
        minimum <- MODEL_DEPENDENT_VAR_DOMAINS[[model]][1]
        maximum <- MODEL_DEPENDENT_VAR_DOMAINS[[model]][2]
        
        # Define SD depending on type of noise
        if (noise.type == "relative") {
            # Standard deviations are proportional to the data
            sds <- abs(synthetic.data[[dependent.var]]) * noise.level
        } else {
            # Standard deviations are always the same (constant)
            sds <- rep(noise.level, num_observations)
        }
        
        # Define the means based on data
        means <- synthetic.data[[dependent.var]]
        
        # Generate noise based on the chosen distribution
        if (distribution == "norm") {
            noisy_data <- rnorm(num_observations, mean = means, sd = sds)
        } else if (distribution == "truncated.norm") {
            noisy_data <- truncnorm::rtruncnorm(num_observations, a = minimum, b = maximum, mean = means, sd = sds)
        }
        
        # Ensure the data is within the range
        clipped_noisy_data <- pmax(pmin(noisy_data, maximum), minimum)
        
        # Warn user if any data was clipped
        if (any(noisy_data != clipped_noisy_data)) {
            warning("Some data values clipped to domain [", minimum, ", ", maximum, "]. Consider using truncated normal distribution.")
        }
        
        # Replace the original data with the noisy data
        synthetic.data[[dependent.var]] <- clipped_noisy_data
    }
    # ===============================
    
    
    return(synthetic.data)
}
