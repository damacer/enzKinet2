## Function file for enzKinet2 package

#' Fit Model
#'
#' @author Haig Bishop
#' 28/06/2024
#'
#' Fits the model to the data. Returns the fitted.params (NULL if could not fit)
#' @param model The chosen model ("MM", "MMSI", etc.)
#' @param data.df The data to fit the model to
#' @param start.params The unfitted parameters for the model (e.g. Km, Vmax)
#' @param override_data_point_check Boolean to override num data points checks.
#' @return fitted.params
#' 
#' @export

fit_model <- function(model, data.df, start.params, override_data_point_check = FALSE) {
    
    # Error Handling ================
    # Check if model is valid
    if (!model %in% VALID_MODELS) {
        stop("Invalid model. Please choose a valid model such as 'MM'.")
    }
    # Check if start.params is a list
    if (!is.list(start.params)) {
        stop("start.params must be a list.")
    }
    # Check if data.df is a dataframe
    if (!is.data.frame(data.df)) {
        stop("data.df must be a dataframe.")
    }
    # Check if data.df is not empty
    if (nrow(data.df) == 0) {
        stop("data.df must not be empty.")
    }
    # Check number of data points
    num_data_points <- nrow(data.df)
    if (!override_data_point_check) {
        if (num_data_points < 5) {
            stop("Less than 5 data points is unlikely insufficient to fit the model. Overide this by setting override_data_point_check to TRUE.")
        } else if (num_data_points < 15) {
            warning("Less than 15 data points may be insufficient to fit the model.")
        }
    }
    # ===============================
    
    
    # Model-specific error handling =======
    # Grab model-specific values
    model.vars <- MODEL_VARIABLES[[model]]
    full.model.name <- PLOT_TITLES[[model]]
    model.vars.string <- MODEL_VARIABLE_STRINGS[[model]]
    # Check if data.df has the necessary columns (variables)
    if (!all(model.vars %in% colnames(data.df))) {
            stop(paste("For the", full.model.name, "model, data.df must contain columns (variables) named", model.vars.string, "."))
    }
    # ===============================
    
    
    # Set up model ready to fit ================
    # Extract the variables according to the chosen model
    param.names <- MODEL_PARAMETERS[[model]]
    full.model.name <- PLOT_TITLES[[model]]
    model.params.string <- MODEL_PARAMETER_STRINGS[[model]]
    
    # Check if required parameters are present
    if (!all(param.names %in% names(start.params))) {
        stop(paste("For the", full.model.name, "model, start.params must include", model.params.string, "."))
    }
    # Check if the parameters are numeric and greater than 0
    for (param in param.names) {
        if (!is.numeric(start.params[[param]])) {
            stop(paste(param, "must be a numeric value."))
        }
        if (start.params[[param]] <= 0) {
            stop(paste(param, "must be greater than 0."))
        }
    }
    
    # Define model
    model.formula <- MODEL_FORMULAE[[model]]
    
    # Define some NLS control parameters
    ctrl <- nls.control(maxiter = 1e5, tol = 1e-5)
    
    # Extract only the needed starting parameters
    start.params <- as.list(start.params[param.names])
    # ===============================
    
    # Fit model =================
    fitted.params <- NULL
    tryCatch({
        fit <- nls(model.formula, data = data.df, start = start.params, control = ctrl)
        fitted.params <- as.list(coef(fit))
        names(fitted.params) <- names(coef(fit))
    }, error = function(e) {
        message("Model fitting failed: ", e$message)
        message("Failiure to fit could be explained by noiseless data, poor starting parameters, over parametrisation , etc..")
        fitted.params <- NULL
    })
    # ===============================
    
    # Return the fitted params (NULL if model could not fit)
    return(fitted.params)
}
