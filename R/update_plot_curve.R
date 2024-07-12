#' Update Plot Curve
#'
#' @author Haig Bishop
#' 08/07/2024
#'
#' Updates (replaces) the curve in an existing enzyme kinetics plot.
#' @param plot The existing plot to be updated
#' @param curve.df Data frame with the new curve data
#' @param model The chosen model ("MM", "MMSI", etc.)
#' @return Updated plot
#' 
#' @export

update_plot_curve <- function(model, plot, curve.df = NULL) {
    
    # Error Handling ================
    # Check if model is valid
    if (!model %in% VALID_MODELS) {
        stop("Invalid model. Please choose a valid model such as 'MM' or 'MMSI'.")
    }
    # Check if curve.df is a dataframe if provided
    if (!is.null(curve.df) && !is.data.frame(curve.df)) {
        stop("curve.df must be a dataframe.")
    }
    # ===============================
    
    # Model-specific error handling =======
    # Grab model-specific values
    model.vars <- MODEL_VARIABLES[[model]]
    dependent.var <- model.vars[2]
    first.independent.var <- model.vars[1]
    full.model.name <- PLOT_TITLES[[model]]
    model.vars.string <- MODEL_VARIABLE_STRINGS[[model]]
    # If curve.df is provided, check if it has the necessary columns (variables)
    if (!is.null(curve.df)) {
        if (!all(model.vars %in% colnames(curve.df))) {
            stop(paste("For the", full.model.name, "model, curve.df must contain columns (variables) named", model.vars.string, "."))
        }
    }
    # ===============================
    
    # Remove the current curve
    plot$layers <- plot$layers[!sapply(plot$layers, function(layer) inherits(layer$geom, "GeomPath"))]
    
    # If a curve(s) was given
    if (!is.null(curve.df)) {
        # If using an extra independent variable
        if (length(model.vars) > 2) {
            # Get the name of that var (e.g. "I")
            extra.independent.variable <- model.vars[3]
            # Set colours
            colours <- paste0("factor(", extra.independent.variable, ")")
            # Split the dataframe into list of multiple df - one per curve
            curve.dfs <- split(curve.df, curve.df[[extra.independent.variable]])
        } else {
            # No extra independent variable
            extra.independent.variable <- NULL
            # Set colours
            colours <- NULL
            # "Split" the dataframe into a list one one df - because only one curve
            curve.dfs <- list(curve.df)
        }
        
        # Loop through each curve df
        for (i in seq_along(curve.dfs)) {
            # Add curve
            plot <- plot + 
                ggplot2::geom_path(data = curve.dfs[[i]], 
                                   ggplot2::aes_string(x = first.independent.var, y = dependent.var, color = colours), 
                                   inherit.aes = FALSE)
        }
    }
    
    return(plot)
}
