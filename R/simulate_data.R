## Function file for enzKinet2 package

#' Simulate kinetics data
#'
#' Generates synthetic data from a model. The values of the independent
#' variables are chosen, the model gives the dependent variable, and random
#' noise is added. Use it to explore a model, to plan an experiment, or to test
#' fitting.
#'
#' @details
#' **Variables.** Each model has a primary independent variable (`A`, the
#' substrate concentration, or `P` for the binding models), a dependent
#' variable (`V`, the rate, or `FB`, the fraction bound) and, for some models,
#' an extra independent variable (`I`, `B` or `R`) whose values come from
#' `z.values`. See [model_registry] for each model's variables and parameters.
#'
#' **Spacing of the x values** (`space`):
#' * `"linear"`: `n.samples` evenly spaced values from `x.min` to `x.max`.
#' * `"exponential"`: denser at low x. Despite the name, the spacing is
#'   quadratic: x = x.min + (x.max - x.min) t^2 for t evenly spaced from 0
#'   to 1.
#' * `"inverse.exponential"`: denser at high x (x = x.min + (x.max - x.min)
#'   (1 - t^3)).
#' * `"dilution.series"`: starts at `x.max` and divides by `dilution.factor`
#'   at each step, stopping after `n.samples` values or before going below
#'   `x.min`.
#' * `"custom"`: the values in `x.range`.
#'
#' **Noise** is added to the dependent variable only. With
#' `noise.type = "relative"` its standard deviation is `noise.level` times the
#' true value, so 0.05 means 5%. With `"absolute"` it is `noise.level`, in the
#' dependent variable's units. With `distribution = "norm"`, noisy values
#' outside the dependent variable's valid range (V at least 0, FB from 0 to 1)
#' are clipped to the edge of the range, with a warning. `"truncated.norm"`
#' draws from a normal distribution truncated to the valid range instead, so
#' values don't pile up at the edge.
#'
#' The noise is random: call [set.seed()] first for reproducible data.
#'
#' @param model The model's code, one of [VALID_MODELS], e.g. `"MM"` or `"CI"`.
#' @param params A named list of parameter values, each greater than 0, e.g.
#'   `list(Km = 0.5, Vmax = 2)`. It must include every parameter in
#'   `MODEL_PARAMETERS[[model]]`. Other elements are ignored.
#' @param x.min,x.max The range of the primary independent variable (`A` or
#'   `P`). `x.min` must be 0 or more, and less than `x.max`.
#' @param z.values For models with an extra independent variable (`I`, `B` or
#'   `R`), a numeric vector of its values. Data is generated for every
#'   combination of x and z. The values must be 0 or more (more than 0 for `R`
#'   in `"QBK"`). Ignored for other models.
#' @param n.samples The number of distinct x values (default 24). For
#'   `"dilution.series"` it's the maximum. Ignored for `"custom"`.
#' @param n.replicates The number of replicates of each point (default 1).
#' @param noise.level The size of the noise (default 0.05). 0 gives noise-free
#'   data. See Details.
#' @param noise.type `"relative"` (default) or `"absolute"`. See Details.
#' @param space How the x values are spaced: `"linear"` (default),
#'   `"exponential"`, `"inverse.exponential"`, `"dilution.series"` or
#'   `"custom"`. See Details.
#' @param dilution.factor For `space = "dilution.series"`, the factor (more
#'   than 1) between successive concentrations, e.g. 2 for a two-fold series.
#' @param x.range For `space = "custom"`, the x values to use: at least two,
#'   all between `x.min` and `x.max`.
#' @param distribution The noise distribution: `"norm"` (default; normal,
#'   clipped to the valid range) or `"truncated.norm"` (truncated normal). See
#'   Details.
#' @return A data frame with a row per point and a column per variable: the
#'   primary independent variable (`A` or `P`), the extra independent variable
#'   if the model has one (`I`, `B` or `R`), then the dependent variable (`V` or
#'   `FB`). Replicates are stacked: every point of the first replicate, then
#'   the second, and so on.
#' @seealso [make_curve()] for noise-free curves, [fit_model()] to fit the
#'   data, [make_plot()] to plot it.
#' @author Haig Bishop
#' @examples
#' set.seed(1)
#' # Michaelis-Menten data: 8 concentrations in triplicate, with 5% noise
#' mm <- simulate_data("MM", params = list(Km = 0.5, Vmax = 2),
#'                     x.min = 0.1, x.max = 5, n.samples = 8, n.replicates = 3)
#' head(mm)
#'
#' # A two-fold dilution series from 10 down to 0.1, with constant noise
#' simulate_data("MM", list(Km = 0.5, Vmax = 2), x.min = 0.1, x.max = 10,
#'               space = "dilution.series", dilution.factor = 2, n.samples = 8,
#'               noise.level = 0.05, noise.type = "absolute")
#'
#' # Competitive inhibition at three inhibitor concentrations
#' ci <- simulate_data("CI", list(Km = 0.5, Vmax = 2, Ki = 1),
#'                     x.min = 0.1, x.max = 5, z.values = c(0, 1, 2), n.samples = 6)
#' make_plot("CI", data.df = ci)
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
    # Check if x.min is non-negative
    if (x.min < 0) {
        stop("x.min must be a non-negative value.")
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
        if (!is.numeric(z.values)) {
            stop("z.values must be a numeric vector.")
        }
        if (any(z.values < 0)) {
            stop("All values in z.values must be greater than or equal to 0.")
        }
        if (model == "QBK" && any(z.values <= 0)) {
            stop("For the Quadratic Binding Kinetics model, R values must be greater than 0.")
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
        # Flip it so points are denser near x.max
        exp_seq <- (exp_seq - 1) * -1
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
        # Zero-width noise (e.g. relative noise at V = 0): keep the mean. rtruncnorm(sd = 0) returns NaN.
        zero.sd <- !is.na(sds) & sds == 0
        if (any(zero.sd)) {
            noisy_data[zero.sd] <- means[zero.sd]
        }
        if (any(!is.finite(noisy_data))) {
            noisy_data[!is.finite(noisy_data)] <- means[!is.finite(noisy_data)]
        }
        
        # Ensure the data is within the range.
        # Clipping one-sided (e.g. FB to 1) biases the mean of simulated data relative to the true curve.
        clipped_noisy_data <- pmax(pmin(noisy_data, maximum), minimum)
        
        # Warn user if any data was clipped
        if (any(is.finite(noisy_data) & noisy_data != clipped_noisy_data)) {
            warning("Some data values clipped to domain [", minimum, ", ", maximum, "]. Consider using truncated normal distribution.")
        }
        
        # Replace the original data with the noisy data
        synthetic.data[[dependent.var]] <- clipped_noisy_data
    }
    # ===============================
    
    
    return(synthetic.data)
}
