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
    # If data.df is provided, check if it has the necessary columns
    if (!is.null(data.df)) {
        if (!all(c("A", "V0") %in% colnames(data.df))) {
            stop("data.df must contain columns named 'A' and 'V0'.")
        }
    }
    # If curve.df is provided, check if it has the necessary columns
    if (!is.null(curve.df)) {
        if (!all(c("A", "V0") %in% colnames(curve.df))) {
            stop("curve.df must contain columns named 'A' and 'V0'.")
        }
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
    
    
    # Initialise Default Plot ===================
    
    # Grab the default title from dictionary
    default.title <- PLOT_TITLES[[model]]
    
    # Make default plot
    plot <- ggplot2::ggplot() + 
        ggplot2::xlab(sprintf("Substrate Concentration [A]")) +
        ggplot2::ylab("Velocity V0") +
        ggplot2::ggtitle(default.title) +
        ggplot2::labs(colour = "Legend") +
        ggthemes::theme_few()
    # ===============================
    
    
    # Add Extra Plot Features ===================
    # If data was given
    if (!is.null(data.df)) {
        # Add the data
        plot <- plot + ggplot2::geom_point(data.df, , mapping = ggplot2::aes(A,V0))
    }
    # If a curve was given
    if (!is.null(curve.df)) {
        # Add the curve
        plot <- plot + ggplot2::geom_line(curve.df, mapping = ggplot2::aes(A,V0), inherit.aes = F)
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
