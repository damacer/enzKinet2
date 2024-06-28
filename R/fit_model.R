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
#' @return fitted.params
#' 
#' @export

fit_model <- function(model, data.df, start.params) {
    
    # Error Handling ================
    # Check if model is valid
    if (!model %in% VALID_MODELS) {
        stop("Invalid model. Please choose a valid model such as 'MM'.")
    }
    # If data.df is provided, check if it has the necessary columns
    if (!is.null(data.df)) {
        if (!all(c("A", "V0") %in% colnames(data.df))) {
            stop("data.df must contain columns named 'A' and 'V0'.")
        }
    }
    # Check if start.params is a list
    if (!is.list(start.params)) {
        stop("start.params must be a list.")
    }
    # ===============================
    
    
    # Set up model ready to fit ================
    # Extract the variables according to the chosen model
    param.names <- MODEL_PARAMETERS[[model]]
    full.model.name <- PLOT_TITLES[[model]]
    model.params.string <- MODEL_PARAMETER_STRINGS[[model]]
    
    # Check if required parameters are present
    if (!all(param.names %in% names(start.params))) {
        stop("For the " + full.model.name + " model, start.params must include " + model.params.string + ".")
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
        fitted.params <- NULL
    })
    # ===============================
    
    
    # Return the fitted params (NULL if model could not fit)
    return(fitted.params)
}
