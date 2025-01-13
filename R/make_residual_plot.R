## Function file for enzKinet2 package

#' Make a Residual Plot
#'
#' @author Haig Bishop
#' 07/09/2024
#'
#' Generates a residual plot for an enzyme kinetics model given parameters and data.
#' @param model The model we are plotting ("MM", "CI", etc.).
#' @param params The parameter values for the model (e.g. Km, Vmax, Ksi).
#' @param data.df The data to calculate residuals from.
#' @param x.max Defines the maximum x value to plot to.
#' @param x.label Custom x-axis label.
#' @param y.label Custom y-axis label.
#' @param x.units Units for the x-axis, such as "mM" or "µM". Defaults to NULL (no units).
#' @param y.units Units for the y-axis, such as "µmol/s" or "U/mL". Defaults to NULL (no units).
#' @param title Custom plot title.
#' @param legend.label Custom legend label.
#' @param zero.line If TRUE (default), draws a dotted line at y=0.
#' @param palette Custom plot colour palette.
#' @param hide.legend Boolean to hide the plot legend.
#' @param y.range Specify the y-axis range (e.g., c(0, 10)). Defaults to NULL (auto range).
#' @return plot
#' 
#' @export

make_residual_plot <- function(model, params, data.df, x.max, x.label = NULL, 
                               y.label = NULL, x.units = NULL, y.units = NULL, 
                               title = NULL, zero.line = TRUE, 
                               legend.label = NULL, palette = "Set1", hide.legend = FALSE,
                               y.range = NULL) {
    
    # Error Handling ================
    # Check if model is valid
    if (!model %in% VALID_MODELS) {
        stop("Invalid model. Please choose a valid model such as 'MM' or 'MMSI'.")
    }
    # Check if params is a list
    if (!is.list(params)) {
        stop("params must be a list.")
    }
    # Check if params is not empty
    if (length(params) == 0) {
        stop("params list cannot be empty.")
    }
    # Check if data.df is a dataframe
    if (!is.data.frame(data.df)) {
        stop("data.df must be a dataframe.")
    }
    # Ensure data.df is not an empty dataframe (if provided)
    if (!is.null(data.df) && nrow(data.df) == 0) {
        stop("data.df must contain at least one row of data.")
    }
    # Check if x.max is numeric
    if (!is.numeric(x.max)) {
        stop("x.max must be a numeric value.")
    }
    # Check if x.max is positive
    if (x.max <= 0) {
        stop("x.max must be a positive value.")
    }
    # Check if x.label, y.label, title and legend.label are characters if provided
    if (!is.null(x.label) && !is.character(x.label)) {
        stop("x.label must be a character string.")
    }
    if (!is.null(y.label) && !is.character(y.label)) {
        stop("y.label must be a character string.")
    }
    if (!is.null(x.units) && !is.character(x.units)) {
        stop("x.units must be a character string.")
    }
    if (!is.null(y.units) && !is.character(y.units)) {
        stop("y.units must be a character string.")
    }
    if (!is.null(title) && !is.character(title)) {
        stop("title must be a character string.")
    }
    if (!is.null(legend.label) && !is.character(legend.label)) {
        stop("legend.label must be a character string.")
    }
    if (!is.null(y.range)) {
        if (!is.numeric(y.range) || length(y.range) != 2) {
            stop("y.range must be a numeric vector of length 2, e.g., c(0, 10).")
        }
        if (y.range[1] >= y.range[2]) {
            stop("y.range must specify a valid range where the first value is less than the second.")
        }
    }
    # ===============================
    
    
    # Model-specific error handling =======
    # Grab model-specific values
    model.vars <- MODEL_VARIABLES[[model]]
    param.names <- MODEL_PARAMETERS[[model]]
    dependent.var <- model.vars[2]
    first.independent.var <- model.vars[1]
    full.model.name <- PLOT_TITLES[[model]]
    model.vars.string <- MODEL_VARIABLE_STRINGS[[model]]
    model.params.string <- MODEL_PARAMETER_STRINGS[[model]]
    # Check if data.df has the necessary columns (variables)
    if (!all(model.vars %in% colnames(data.df))) {
        stop(paste("For the", full.model.name, "model, data.df must contain columns (variables) named", model.vars.string, "."))
    }
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
    # ===============================
    
    
    # Calculate Residuals ================
    # If using an extra independent variable
    if (length(model.vars) > 2) {
        # Get the name of that var (e.g. "I")
        extra.independent.variable <- model.vars[3]
        # Get a vector of the unique values of that in data.df
        z.values <- unique(data.df[[extra.independent.variable]])
    } else {
        # No extra independent variable
        extra.independent.variable <- NULL
        # No values of z
        z.values <- NULL
    }
    
    # Get formula 
    model.formula <- MODEL_FORMULAE[[model]]
    # Make a function to compute perfect data
    calculate_dependent_var <- function(row, formula, params, independent.var, extra.var = NULL) {
        env <- list2env(params)
        env[[independent.var]] <- row[[independent.var]]
        if (!is.null(extra.var)) {
            env[[extra.var]] <- row[[extra.var]]
        }
        return(eval(formula[[3]], envir = env))
    }
    # Make copy of data
    perfect.data.df <- data.df
    # Repopulate the dependent.var column using params, first.independent.var, and (if applicable) extra.independent.variable
    perfect.data.df[[dependent.var]] <- apply(perfect.data.df, 1, calculate_dependent_var, 
                                              formula = model.formula, 
                                              params = params, 
                                              independent.var = first.independent.var, 
                                              extra.var = extra.independent.variable)
    # Calculate residuals
    residuals.df <- data.df
    residuals.df[[dependent.var]] <- data.df[[dependent.var]] - perfect.data.df[[dependent.var]]
    
    # Determine the range of residuals for a symmetrical y-axis centered on y=0
    residual.range <- range(residuals.df[[dependent.var]])
    y.max <- max(abs(residual.range))
    y.limits <- c(-y.max, y.max)
    
    
    # Initialise Plot ===================
    # Grab the default title from dictionary
    default.title <- paste(PLOT_TITLES[[model]], "Residuals")
    default.x.axis <- AXIS_TITLES[[first.independent.var]]
    default.y.axis <- paste("Residual", AXIS_TITLES[[dependent.var]])
    # Append units to axis labels if x.units or y.units are provided
    if (!is.null(x.units) && x.units != "") {
        default.x.axis <- paste0(default.x.axis, " (", x.units, ")")
    }
    if (!is.null(y.units) && y.units != "") {
        default.y.axis <- paste0(default.y.axis, " (", y.units, ")")
    }
    
    # Make default plot
    x.max.data <- max(data.df[[first.independent.var]])
    extra.independent.var <- if (length(model.vars) > 2) model.vars[3] else NULL
    legend_name <- if (!is.null(legend.label)) legend.label else if (!is.null(extra.independent.var)) AXIS_TITLES[[extra.independent.var]] else "Legend"
    plot <- ggplot2::ggplot() + 
        ggplot2::xlab(sprintf(default.x.axis)) +
        ggplot2::ylab(sprintf(default.y.axis)) +
        ggplot2::ggtitle(default.title) +
        ggplot2::labs(color = legend_name) +
        ggthemes::theme_few() + 
        ggplot2::scale_color_brewer(palette = palette) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
        ggplot2::ylim(y.limits) +
        ggplot2::xlim(0, max(x.max, x.max.data))
    # ===============================
    
    
    # Add Data Points ===================
    # If using an extra independent variable
    if (length(model.vars) > 2) {
        # Get the name of that var (e.g. "I")
        extra.independent.variable <- model.vars[3]
        # Set colours
        colours <- paste0("factor(", extra.independent.variable, ")")
    } else {
        # No extra independent variable
        extra.independent.variable <- NULL
        # Set colours
        colours <- "'This is the only colour'"
        # Disable the plot legend as there's only one colour
        plot <- plot + ggplot2::guides(color = "none")
    }
    
    
    # If we are using 2 independent variables
    if (!is.null(extra.independent.variable)) {
        # Split the dataframe into list of multiple df - one per value of the extra independent variable
        residuals.dfs <- split(residuals.df, residuals.df[[extra.independent.variable]])
    } else {
        # "Split" the dataframe into a list of one df - because only one set of points
        residuals.dfs <- list(residuals.df)
    }
    # Loop through each data df
    for (i in seq_along(residuals.dfs)) {
        # Add the data points
        plot <- plot + 
            ggplot2::geom_point(data = residuals.dfs[[i]], 
                    ggplot2::aes_string(x = first.independent.var, y = dependent.var, color = colours), 
                    inherit.aes = FALSE)
    }
    
    # Add Extra Plot Features ===================
    # If an x-axis label was given
    if (!is.null(x.label)) {
        # Append units to axis labels if x.units is provided
        if (!is.null(x.units) && x.units != "") {
            x.label <- paste0(x.label, " (", x.units, ")")
        }
        # Add the label
        plot <- plot + ggplot2::xlab(sprintf(x.label))
    }
    # If an y-axis label was given
    if (!is.null(y.label)) {
        # Append units to axis labels if y.units is provided
        if (!is.null(y.units) && y.units != "") {
            y.label <- paste0(y.label, " (", y.units, ")")
        }
        # Add the label
        plot <- plot + ggplot2::ylab(sprintf(y.label))
    }
    # If a title was given
    if (!is.null(title)) {
        # Add the title
        plot <- plot + ggplot2::ggtitle(sprintf(title))
    }
    # Logic for hide.legend parameter
    if (hide.legend) {
        plot <- plot + ggplot2::theme(legend.position = "none")
    }
    # If a y range was given
    if (!is.null(y.range)) {
        plot <- plot + ggplot2::scale_y_continuous(limits = y.range)
    }
    # ===============================
    
    
    return(plot)
}
