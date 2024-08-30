## Function file for enzKinet2 package

#' Make a Residual Plot
#'
#' @author Haig Bishop
#' 28/07/2024
#'
#' Generates a residual plot for an enzyme kinetics model given parameters and data.
#' @param model The chosen model ("MM", "MMSI", etc.)
#' @param params The parameters for the model (Including Km, Vmax, Ksi, etc.)
#' @param data.df The data which might fit the curve
#' @param x.max Defines the maximum of x value.
#' @param x.label Custom x-axis label.
#' @param y.label Custom y-axis label.
#' @param title Custom plot title.
#' @param zero.line If TRUE, draws a dotted line at y=0.
#' @param palette Custom plot colour palette.
#' @param hide_legend Boolean to hide the plot legend.
#' @return plot
#' 
#' @export

make_residual_plot <- function(model, params, data.df, x.max, x.label = NULL, 
                               y.label = NULL, title = NULL, zero.line = TRUE, 
                               palette = "Set1", hide_legend = FALSE) {
    
    # Error Handling ================
    # Check if model is valid
    if (!model %in% VALID_MODELS) {
        stop("Invalid model. Please choose a valid model such as 'MM' or 'MMSI'.")
    }
    # Check if params is a list
    if (!is.list(params)) {
        stop("params must be a list.")
    }
    # Check if data.df is a dataframe
    if (!is.data.frame(data.df)) {
        stop("data.df must be a dataframe.")
    }
    # Check if x.max is numeric
    if (!is.numeric(x.max)) {
        stop("x.max must be a numeric value.")
    }
    # Check if x.max is positive
    if (x.max < 0) {
        stop("x.max must be a positive value.")
    }
    # Check if x.label, y.label, and title are characters if provided
    if (!is.null(x.label) && !is.character(x.label)) {
        stop("x.label must be a character string.")
    }
    if (!is.null(y.label) && !is.character(y.label)) {
        stop("y.label must be a character string.")
    }
    if (!is.null(title) && !is.character(title)) {
        stop("title must be a character string.")
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
    
    # Make copy of data
    perfect.data.df <- data.df
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
    
    # Make default plot
    x.max.data <- max(data.df[[first.independent.var]])
    extra.independent.var <- if (length(model.vars) > 2) model.vars[3] else "Legend"
    legend_name <- AXIS_TITLES[[extra.independent.var]]
    plot <- ggplot2::ggplot() + 
        ggplot2::xlab(sprintf(default.x.axis)) +
        ggplot2::ylab(default.y.axis) +
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
        # Turn off the legend
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
        # Add the label
        plot <- plot + ggplot2::xlab(sprintf(x.label))
    }
    # If an y-axis label was given
    if (!is.null(y.label)) {
        # Add the label
        plot <- plot + ggplot2::xlab(sprintf(y.label))
    }
    # If a title was given
    if (!is.null(title)) {
        # Add the title
        plot <- plot + ggplot2::ggtitle(sprintf(title))
    }
    # Logic for hide_legend parameter
    if (hide_legend) {
        plot <- plot + ggplot2::theme(legend.position = "none")
    }
    # ===============================
    
    
    return(plot)
}
