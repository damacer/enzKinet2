## Function file for enzKinet2 package

#' Make Plot
#'
#' @author Haig Bishop
#' 28/06/2024
#'
#' Generates a plot for an enzyme kinetics model.
#' @param model The chosen model ("MM", "MMSI", etc.)
#' @param data.df Data plotted as dots
#' @param curve.df Dense data used to draw the model curve 
#' @param x.label Custom x-axis label.
#' @param y.label Custom y-axis label.
#' @param title Custom plot title.
#' @param extra.curve An extra curve to be displayed on top of the other
#' @return plot
#' 
#' @export

make_plot <- function(model, data.df = NULL, curve.df = NULL, x.label = NULL, y.label = NULL, title = NULL, extra.curve = NULL) {
    
    # Error Handling ================
    # Check if model is valid
    if (!model %in% VALID_MODELS) {
        stop("Invalid model. Please choose a valid model such as 'MM' or 'MMSI'.")
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
    # Check if data.df is a dataframe if provided
    if (!is.null(data.df) && !is.data.frame(data.df)) {
        stop("data.df must be a dataframe.")
    }
    # Check if curve.df is a dataframe if provided
    if (!is.null(curve.df) && !is.data.frame(curve.df)) {
        stop("curve.df must be a dataframe.")
    }
    # Check if extra.curve is a dataframe if provided
    if (!is.null(extra.curve) && !is.data.frame(extra.curve)) {
        stop("extra.curve must be a dataframe.")
    }
    # ===============================
    
    
    # Model-specific error handling =======
    # Grab model-specific values
    model.vars <- MODEL_VARIABLES[[model]]
    dependent.var <- model.vars[2]
    first.independent.var <- model.vars[1]
    full.model.name <- PLOT_TITLES[[model]]
    model.vars.string <- MODEL_VARIABLE_STRINGS[[model]]
    # If data.df is provided, check if it has the necessary columns (variables)
    if (!is.null(data.df)) {
        if (!all(model.vars %in% colnames(data.df))) {
            stop(paste("For the", full.model.name, "model, data.df must contain columns (variables) named", model.vars.string, "."))
        }
    }
    # If curve.df is provided, check if it has the necessary columns (variables)
    if (!is.null(curve.df)) {
        if (!all(model.vars %in% colnames(curve.df))) {
            stop(paste("For the", full.model.name, "model, curve.df must contain columns (variables) named", model.vars.string, "."))
        }
    }
    # If extra.curve is provided, check if it has the necessary columns (variables)
    if (!is.null(extra.curve)) {
        if (!all(model.vars %in% colnames(extra.curve))) {
            stop(paste("For the", full.model.name, "model, extra.curve must contain columns (variables) named", model.vars.string, "."))
        }
    }
    # ===============================
    
    
    # Initialise Default Plot ===================
    
    # Grab the default title from dictionary
    default.title <- PLOT_TITLES[[model]]
    default.x.axis <- AXIS_TITLES[[first.independent.var]]
    default.y.axis <- AXIS_TITLES[[dependent.var]]
    
    # Make default plot
    plot <- ggplot2::ggplot() + 
        ggplot2::xlab(sprintf(default.x.axis)) +
        ggplot2::ylab(default.y.axis) +
        ggplot2::ggtitle(default.title) +
        ggplot2::labs(color = "Legend") +
        ggthemes::theme_few()
    
    # Get the valid domain of the dependent variable
    dependent.var.domain <- MODEL_DEPENDENT_VAR_DOMAINS[[model]]
    # If it doesn't include infinity
    if (!Inf %in% dependent.var.domain) {
        # Set y-axis range to it (e.g.between 0 and 1)
        plot <- plot + ggplot2::scale_y_continuous(limits = dependent.var.domain)
    }
    # ===============================
    
    
    # Add Extra Plot Features ===================
    
    # Add colour pallette
    plot <- plot + ggplot2::scale_color_brewer(palette = "Set1")
    
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
    
    # If data was given
    if (!is.null(data.df)) {
        # If we are using 2 independent variables
        if (!is.null(extra.independent.variable)) {
            # Split the dataframe into list of multiple df - one per value of the extra independent variable
            data.dfs <- split(data.df, data.df[[extra.independent.variable]])
        } else {
            # "Split" the dataframe into a list of one df - because only one set of points
            data.dfs <- list(data.df)
        }
        # Loop through each data df
        for (i in seq_along(data.dfs)) {
            # Add the data points
            plot <- plot + 
                ggplot2::geom_point(data = data.dfs[[i]], 
                        ggplot2::aes_string(x = first.independent.var, y = dependent.var, color = colours), 
                        inherit.aes = FALSE)
        }
    }
    # If a curve(s) was given
    if (!is.null(curve.df)) {
        # If we are using 2 independent variables
        if (!is.null(extra.independent.variable)) {
            # Split the dataframe into list of multiple df - one per curve
            curve.dfs <- split(curve.df, curve.df[[extra.independent.variable]])
        } else {
            # "Split" the dataframe into a list one one df - because only one curve
            curve.dfs <- list(curve.df)
        }
        # Loop through each curve df
        for (i in seq_along(curve.dfs)) {
            # Draw the curve
            plot <- plot + 
                ggplot2::geom_path(data = curve.dfs[[i]], 
                                   ggplot2::aes_string(x = first.independent.var, y = dependent.var, color = colours), 
                                   inherit.aes = FALSE)
        }
    }
    # If an extra curve(s) was given
    if (!is.null(extra.curve)) {
        # If we are using 2 independent variables
        if (!is.null(extra.independent.variable)) {
            # Split the dataframe into list of multiple df - one per curve
            curve.dfs <- split(extra.curve, extra.curve[[extra.independent.variable]])
        } else {
            # "Split" the dataframe into a list one one df - because only one curve
            curve.dfs <- list(extra.curve)
        }
        # Loop through each curve df
        for (i in seq_along(curve.dfs)) {
            # Draw the curve
            plot <- plot + 
                ggplot2::geom_path(data = curve.dfs[[i]], 
                                   ggplot2::aes_string(x = first.independent.var, y = dependent.var, color = NULL), 
                                   inherit.aes = FALSE)
        }
    }
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
    # ===============================
    
    
    return(plot)
}
