## Function file for enzKinet2 package

#' Replace the curve in a plot
#'
#' Removes the curves and envelope from a plot made by [make_plot()] and draws
#' new ones, keeping the data points and the plot's other settings. This is
#' quicker than redrawing the whole plot, e.g. as parameter values change.
#'
#' @param model The model's code, as used to make `plot`.
#' @param plot A plot made by [make_plot()].
#' @param curve.df The new curve, from [make_curve()]. Optional. Without it,
#'   the plot is left with no curve.
#' @param extra.curve A new extra curve, drawn in black. Optional.
#' @param plot.transformation The transformation that `plot` was made with
#'   (default `"standard"`). It must match, because the new curve is
#'   transformed in the same way. A `"direct"` plot has no curves to replace.
#' @param conf.int If `TRUE`, shade the envelope from `curve.df` (see
#'   [make_plot()]). Default `FALSE`.
#' @param palette The colour palette that `plot` was made with (default
#'   `"Set1"`).
#' @param y.range The y-axis limits, a numeric vector of two values. Default
#'   `NULL`.
#' @return The updated ggplot object.
#' @seealso [make_plot()], [make_curve()].
#' @author Haig Bishop
#' @examples
#' set.seed(1)
#' mm <- simulate_data("MM", list(Km = 0.5, Vmax = 2), x.min = 0.1, x.max = 5,
#'                     n.samples = 8)
#' p <- make_plot("MM", data.df = mm,
#'                curve.df = make_curve("MM", list(Km = 0.5, Vmax = 2), 0, 5))
#' update_plot_curve("MM", p,
#'                   curve.df = make_curve("MM", list(Km = 1, Vmax = 2.5), 0, 5))
#' @export

update_plot_curve <- function(model, plot, curve.df = NULL, extra.curve = NULL, 
                              plot.transformation = "standard",  conf.int = FALSE,
                              y.range = NULL, palette = "Set1") {
    
    # Error Handling ================
    # Check if model is valid
    if (!model %in% VALID_MODELS) {
        stop("Invalid model. Please choose a valid model such as 'MM' or 'MMSI'.")
    }
    # Check if plot is a ggplot object
    if (!inherits(plot, "gg")) {
        stop("plot must be a ggplot object.")
    }
    # Check if curve.df is a dataframe if provided
    if (!is.null(curve.df) && !is.data.frame(curve.df)) {
        stop("curve.df must be a dataframe.")
    }
    # Check if extra.curve is a dataframe if provided
    if (!is.null(extra.curve) && !is.data.frame(extra.curve)) {
        stop("extra.curve must be a dataframe.")
    }
    # Check if conf.int is a logical
    if (!is.logical(conf.int)) {
        stop("conf.int must be TRUE or FALSE.")
    }
    # Check if confidence envelope curves were provided (if used)
    if (conf.int && is.null(curve.df)) {
        stop("Confidence envelope requested, but curve.df is missing.")
    }
    if (conf.int && !all(CONFIDENCE_INTERVAL_BOUNDING_VARIABLES[[model]] %in% colnames(curve.df))) {
        stop("Parameter confidence envelope curves (e.g. 'V.lb', 'V.ub') not provided.")
    }
    # Eadie maps x to V/A so the lower and upper curves move sideways. A vertical ribbon at the main curve's x is not the envelope.
    if (plot.transformation == "eadie") {
        conf.int <- FALSE
    }
    # Check if plot.transformation is valid
    if (!plot.transformation %in% names(PLOT_TRANSFORMATIONS)) {
        stop("plot.transformation is invalid.")
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
    if (!palette %in% PLOT_COLOUR_PALETTES) {
        stop("Invalid colour palette. Please choose a valid palette from: ", paste(PLOT_COLOUR_PALETTES, collapse = ", "))
    }
    # ===============================
    
    # Model-specific error handling =======
    # Grab model-specific values
    model.vars <- MODEL_VARIABLES[[model]]
    dependent.var <- model.vars[2]
    first.independent.var <- model.vars[1]
    full.model.name <- PLOT_TITLES[[model]]
    model.vars.string <- MODEL_VARIABLE_STRINGS[[model]]
    # Check if plot.transformation is blocked for this model
    if (plot.transformation %in% BLOCKED_TRANSFORMATIONS[[model]]) {
        stop(paste("The", plot.transformation, "transformation is not supported for the", full.model.name, "model."))
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
    # If direct linear plot, there shouldn't be any curves
    if (plot.transformation == "direct") {
        if (!is.null(curve.df) || !is.null(extra.curve)) {
            stop("For a direct linear plot, curves are not used.")
        }
    }
    # ===============================
    
    # Remove the current curve and confidence envelope
    plot$layers <- plot$layers[!sapply(plot$layers, function(layer) {
        inherits(layer$geom, "GeomPath") || inherits(layer$geom, "GeomRibbon")
    })]
    
    
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

            # Lineweaver (1/V) and Hanes (A/V) are decreasing in V, so what was the
            # lower-V curve is now the higher value in transformed space. Swap labels.
            if (plot.transformation %in% c("lineweaver", "hanes")) {
                names(lb.curve.df) <- sub("\\.lb$", ".ub", names(lb.curve.df))
                names(ub.curve.df) <- sub("\\.ub$", ".lb", names(ub.curve.df))
            }

            # Reintegrate transformed lower and upper bound curves into curve.df
            curve.df <- cbind(curve.df[, !grepl("\\.lb$|\\.ub$", names(curve.df))], lb.curve.df, ub.curve.df)
        }
        curve.df <- drop_nonfinite_xy(curve.df, first.independent.var, dependent.var, plot.transformation)
    }
    if (!is.null(extra.curve)) {
        extra.curve = transformation(extra.curve, x.name = first.independent.var, y.name = dependent.var)
        extra.curve <- drop_nonfinite_xy(extra.curve, first.independent.var, dependent.var, plot.transformation)
    }
    # ===============================
    
    
    # If using an extra independent variable
    if (length(model.vars) > 2) {
        extra.independent.variable <- model.vars[3]
    } else {
        extra.independent.variable <- NULL
        plot <- plot + ggplot2::guides(color = "none")
    }
    
    # Confidence Intervals ============
    if (conf.int) {
        # If we are using 2 independent variables
        if (!is.null(extra.independent.variable)) {
            curve.df <- curve.df[order(curve.df[[extra.independent.variable]]), ]
            curve.dfs <- split(curve.df, curve.df[[extra.independent.variable]])
        } else {
            curve.dfs <- list(curve.df)
        }
        for (i in seq_along(curve.dfs)) {
            plot <- plot + ggplot2::geom_ribbon(
                data = curve.dfs[[i]],
                ribbon_aes(first.independent.var, dependent.var, extra.independent.variable),
                alpha = 0.3, inherit.aes = FALSE,
                show.legend = FALSE
            )
        }
        plot$scales$scales <- plot$scales$scales[!sapply(plot$scales$scales, function(s) "fill" %in% s$aesthetics)]
        n.series <- if (!is.null(extra.independent.variable)) length(unique(curve.df[[extra.independent.variable]])) else 1
        plot <- plot + colour_scale(palette, n.series, aesthetic = "fill")
        # Now we can for sure remove upper and lower curves to prevent issues later in the function
        curve.df <- curve.df[, !grepl("\\.lb$|\\.ub$", names(curve.df))]
    }
    # =================================
    
    
    # If a curve(s) was given
    if (!is.null(curve.df)) {
        # If using an extra independent variable
        if (!is.null(extra.independent.variable)) {
            # Split the dataframe into list of multiple df - one per curve
            curve.dfs <- split(curve.df, curve.df[[extra.independent.variable]])
        } else {
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
                                   colour_aes(first.independent.var, dependent.var, extra.independent.variable), 
                                   inherit.aes = FALSE)
        }
    }
    # If an extra curve was given
    if (!is.null(extra.curve)) {
        # If using an extra independent variable
        if (!is.null(extra.independent.variable)) {
            # Split the dataframe into list of multiple df - one per curve
            curve.dfs <- split(extra.curve, extra.curve[[extra.independent.variable]])
        } else {
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
                                   ggplot2::aes(x = .data[[first.independent.var]], y = .data[[dependent.var]]), 
                                   inherit.aes = FALSE)
        }
    }
    # If a y range was given
    if (!is.null(y.range)) {
        plot <- plot + ggplot2::scale_y_continuous(limits = y.range)
    }
    
    return(plot)
}
