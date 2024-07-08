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
#' @param x.label Defines the range of x values to cover.
#' @param y.label Defines the range of x values to cover.
#' @param title Defines the range of x values to cover.
#' @return plot
#' 
#' @export

make_plot <- function(model, data.df = NULL, curve.df = NULL, x.label = NULL, y.label = NULL, title = NULL) {
    
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
    # ===============================
    
    
    # Model-specific error handling =======
    # Grab model-specific values
    model.vars <- MODEL_VARIABLES[[model]]
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
    # ===============================
    
    
    # Initialise Default Plot ===================
    
    # Grab the default title from dictionary
    default.title <- PLOT_TITLES[[model]]
    
    # Make default plot
    plot <- ggplot2::ggplot() + 
        ggplot2::xlab(sprintf("Substrate Concentration [A]")) +
        ggplot2::ylab("Velocity V") +
        ggplot2::ggtitle(default.title) +
        ggplot2::labs(color = "Legend") +
        ggthemes::theme_few()
    # ===============================
    
    
    # Add Extra Plot Features ===================
    
    # If using an extra independent variable
    if (length(model.vars) > 2) {
        # Get the name of that var (e.g. "I")
        extra.independent.variable <- model.vars[3]
        # Set colours
        colours <- paste0("factor(", extra.independent.variable, ")")
        # Add colour pallette
        plot <- plot + ggplot2::scale_color_brewer(palette = "Set1")
    } else {
        # No extra independent variable
        extra.independent.variable <- NULL
        # Set colours
        colours <- NULL
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
                        ggplot2::aes_string(x = "A", y = "V", color = colours), 
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
                        ggplot2::aes_string(x = "A", y = "V", color = colours), 
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
