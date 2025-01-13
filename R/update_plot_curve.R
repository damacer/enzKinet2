## Function file for enzKinet2 package

#' Update Plot Curve
#'
#' @author Haig Bishop
#' 07/09/2024
#'
#' Updates (replaces) the curve in an existing enzyme kinetics plot.
#' @param plot The existing plot to be updated
#' @param curve.df Data frame with the new curve data
#' @param model The chosen model ("MM", "MMSI", etc.)
#' @param extra.curve An extra curve to be displayed on top of the other
#' @param plot.transformation Transformation to give the data.
#' @param conf.int Boolean for whether or not to plot confidence intervals.
#' @param y.range Specify the y-axis range (e.g., c(0, 10)). Defaults to NULL (auto range).
#' @return Updated plot
#' 
#' @export

update_plot_curve <- function(model, plot, curve.df = NULL, extra.curve = NULL, 
                              plot.transformation = "standard",  conf.int = FALSE,
                              y.range = NULL) {
    
    # Error Handling ================
    # Check if model is valid
    if (!model %in% VALID_MODELS) {
        stop("Invalid model. Please choose a valid model such as 'MM' or 'MMSI'.")
    }
    # Check if curve.df is a dataframe if provided
    if (!is.null(curve.df) && !is.data.frame(curve.df)) {
        stop("curve.df must be a dataframe.")
    }
    # Check if extra.curve is a dataframe if provided
    if (!is.null(extra.curve) && !is.data.frame(extra.curve)) {
        stop("extra.curve must be a dataframe.")
    }
    # Check if confidence interval curves were provided (if used)
    if (conf.int && is.null(curve.df)) {
        stop("Confidence intervals requested, but curve.df is missing.")
    }
    # Check if confidence interval curves were provided (if used)
    if (conf.int && !all(CONFIDENCE_INTERVAL_BOUNDING_VARIABLES[[model]] %in% colnames(curve.df))) {
        stop("Confidence interval curves (e.g. 'V.lb', 'V.ub') not provided.")
    }
    if (plot.transformation == "direct") {
        if (!is.null(curve.df) || !is.null(extra.curve)) {
            stop("Curves are not used in direct linear plot transformations. Please remove curve.df and extra.curve.")
        }
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
    # If extra.curve is provided, check if it has the necessary columns (variables)
    if (!is.null(extra.curve)) {
        if (!all(model.vars %in% colnames(extra.curve))) {
            stop(paste("For the", full.model.name, "model, extra.curve must contain columns (variables) named", model.vars.string, "."))
        }
    }
    # If direct linear plot, there shouldn't be any curves
    if (plot.transformation == "direct") {
        if (!is.null(curve.df) || !is.null(extra.curve)) {
            stop("For a direct linear plot, curves are not used.")
        }
    }
    # ===============================
    
    # Remove the current curve
    plot$layers <- plot$layers[!sapply(plot$layers, function(layer) inherits(layer$geom, "GeomPath"))]
    
    
    # Transform the curve(s) ===============
    transformation <- PLOT_TRANSFORMATIONS[[plot.transformation]]
    if (!is.null(curve.df)) {
        curve.df = transformation(curve.df, x.name = first.independent.var, y.name = dependent.var)
        if (conf.int) {
            # Transform lower bound curve
            lb.curve.df <- curve.df[, grep("\\.lb$", names(curve.df))]
            names(lb.curve.df) <- sub("\\.lb$", "", names(lb.curve.df))
            lb.curve.df <- transformation(lb.curve.df, x.name = first.independent.var, y.name = dependent.var)
            names(lb.curve.df) <- paste0(names(lb.curve.df), ".lb")
            
            # Transform upper bound curve
            ub.curve.df <- curve.df[, grep("\\.ub$", names(curve.df))]
            names(ub.curve.df) <- sub("\\.ub$", "", names(ub.curve.df))
            ub.curve.df <- transformation(ub.curve.df, x.name = first.independent.var, y.name = dependent.var)
            names(ub.curve.df) <- paste0(names(ub.curve.df), ".ub")
            
            # Reintegrate transformed lower and upper bound curves into curve.df
            curve.df <- cbind(curve.df[, !grepl("\\.lb$|\\.ub$", names(curve.df))], lb.curve.df, ub.curve.df)
        }
    }
    if (!is.null(extra.curve)) {
        extra.curve = transformation(extra.curve, x.name = first.independent.var, y.name = dependent.var)
    }
    # ===============================
    
    
    
    # Confidence Intervals ============
    if (conf.int) {
        # Add a grey confidence interval ribbon to the plot
        plot <- plot + ggplot2::geom_ribbon(
            data = curve.df, 
            ggplot2::aes_string(
                x = first.independent.var, 
                ymin = paste0(dependent.var, ".lb"), 
                ymax = paste0(dependent.var, ".ub")
            ), 
            fill = "grey", alpha = 0.2, inherit.aes = FALSE
        )
        
        # Now we can for sure remove upper and lower curves to prevent issues later in the function
        curve.df <- curve.df[, !grepl("\\.lb$|\\.ub$", names(curve.df))]
    }
    # =================================
    
    
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
            colours <- "'This is the only colour'"
            # Turn off the legend
            plot <- plot + ggplot2::guides(color = "none")
            # "Split" the dataframe into a list one one df - because only one curve
            curve.dfs <- list(curve.df)
        }
        
        # Loop through each curve df
        for (i in seq_along(curve.dfs)) {
            this.curve.df <- curve.dfs[[i]]
            # If this is a lineweaver-burk plot add the x-intercept
            if (plot.transformation == "lineweaver") {
                # Get min and max points
                x.axis <- this.curve.df[[first.independent.var]]
                y.axis <- this.curve.df[[dependent.var]]
                x1 <- min(x.axis, na.rm = TRUE)
                y1 <- y.axis[which.min(x.axis)]
                x2 <- max(x.axis, na.rm = TRUE)
                y2 <- y.axis[which.max(x.axis)]
                # Calculate the slope
                m <- (y2 - y1) / (x2 - x1)
                # Calculate the y-intercept using the formula c = y - m * x
                c <- y1 - m * x1
                # Calculate the x-intercept
                x_intercept = -c / m
                # Create a new point at the x_intercept
                new_row <- this.curve.df[1, ]
                new_row[[first.independent.var]] <- x_intercept
                new_row[[dependent.var]] <- 0
                this.curve.df <- rbind(this.curve.df, new_row)
                # Sort the dataframe
                this.curve.df <- this.curve.df[order(this.curve.df[[first.independent.var]]), ]
            }
            # Add curve
            plot <- plot + 
                ggplot2::geom_path(data = this.curve.df, 
                                   ggplot2::aes_string(x = first.independent.var, y = dependent.var, color = colours), 
                                   inherit.aes = FALSE)
        }
    }
    # If an extra curve was given
    if (!is.null(extra.curve)) {
        # If using an extra independent variable
        if (length(model.vars) > 2) {
            # Get the name of that var (e.g. "I")
            extra.independent.variable <- model.vars[3]
            # Split the dataframe into list of multiple df - one per curve
            curve.dfs <- split(extra.curve, extra.curve[[extra.independent.variable]])
        } else {
            # No extra independent variable
            extra.independent.variable <- NULL
            # "Split" the dataframe into a list one one df - because only one curve
            curve.dfs <- list(extra.curve)
        }
        
        # Loop through each curve df
        for (i in seq_along(curve.dfs)) {
            this.curve.df <- curve.dfs[[i]]
            # If this is a lineweaver-burk plot add the x-intercept
            if (plot.transformation == "lineweaver") {
                # Get min and max points
                x.axis <- this.curve.df[[first.independent.var]]
                y.axis <- this.curve.df[[dependent.var]]
                x1 <- min(x.axis, na.rm = TRUE)
                y1 <- y.axis[which.min(x.axis)]
                x2 <- max(x.axis, na.rm = TRUE)
                y2 <- y.axis[which.max(x.axis)]
                # Calculate the slope
                m <- (y2 - y1) / (x2 - x1)
                # Calculate the y-intercept using the formula c = y - m * x
                c <- y1 - m * x1
                # Calculate the x-intercept
                x_intercept = -c / m
                # Create a new point at the x_intercept
                new_row <- this.curve.df[1, ]
                new_row[[first.independent.var]] <- x_intercept
                new_row[[dependent.var]] <- 0
                this.curve.df <- rbind(this.curve.df, new_row)
                # Sort the dataframe
                this.curve.df <- this.curve.df[order(this.curve.df[[first.independent.var]]), ]
            }
            # Add curve
            plot <- plot + 
                ggplot2::geom_path(data = this.curve.df, 
                                   ggplot2::aes_string(x = first.independent.var, y = dependent.var, color = NULL), 
                                   inherit.aes = FALSE)
        }
    }
    # If a y range was given
    if (!is.null(y.range)) {
        plot <- plot + ggplot2::scale_y_continuous(limits = y.range)
    }
    
    return(plot)
}
