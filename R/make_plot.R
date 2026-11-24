## Function file for enzKinet2 package

drop_nonfinite_xy <- function(df, x.name, y.name, transformation.name) {
    if (is.null(df) || nrow(df) == 0) {
        return(df)
    }
    keep <- is.finite(df[[x.name]]) & is.finite(df[[y.name]])
    if (!all(keep)) {
        warning("Dropped ", sum(!keep), " non-finite row(s) after the ", transformation.name, " transformation.", call. = FALSE)
        df <- df[keep, , drop = FALSE]
    }
    return(df)
}

# Bracket a unit that contains "/" when it is divided by, e.g. 1/(µM/min) rather than 1/µM/min
divisor_unit <- function(units) {
    if (grepl("/", units, fixed = TRUE)) paste0("(", units, ")") else units
}

colour_aes <- function(x.name, y.name, extra.var) {
    if (is.null(extra.var)) {
        ggplot2::aes(x = .data[[x.name]], y = .data[[y.name]], colour = "This is the only colour")
    } else {
        ggplot2::aes(x = .data[[x.name]], y = .data[[y.name]], colour = factor(.data[[extra.var]]))
    }
}

# Return a colour scale appropriate for the number of series.
# Falls back to ggplot2's default hue scale when the palette has fewer colours than needed.
colour_scale <- function(palette, n.series, aesthetic = "colour") {
    max.colours <- tryCatch(
        suppressWarnings(length(RColorBrewer::brewer.pal(n.series, palette))),
        error = function(e) 0
    )
    if (n.series <= max.colours) {
        if (aesthetic == "fill") return(ggplot2::scale_fill_brewer(palette = palette))
        return(ggplot2::scale_color_brewer(palette = palette))
    }
    if (aesthetic == "fill") return(ggplot2::scale_fill_hue())
    return(ggplot2::scale_color_hue())
}

ribbon_aes <- function(x.name, y.name, extra.var) {
    ymin.name <- paste0(y.name, ".lb")
    ymax.name <- paste0(y.name, ".ub")
    if (is.null(extra.var)) {
        ggplot2::aes(x = .data[[x.name]], ymin = .data[[ymin.name]], ymax = .data[[ymax.name]], fill = "This is the only colour")
    } else {
        ggplot2::aes(x = .data[[x.name]], ymin = .data[[ymin.name]], ymax = .data[[ymax.name]], fill = factor(.data[[extra.var]]))
    }
}

#' Plot kinetics data and curves
#'
#' Draws data points and model curves, with or without a linearising
#' transformation, and returns a ggplot that you can print, save or change
#' further.
#'
#' @details
#' **Transformations** (`plot.transformation`). The data and the curves are
#' transformed in the same way, so a transformed plot shows the model's curve
#' rather than a straight line fitted to the transformed points.
#' * `"standard"`: v against a.
#' * `"log10"`: v against log10(a).
#' * `"lineweaver"`: the Lineweaver-Burk plot, 1/v against 1/a. Each curve is
#'   extended to the 1/a axis, and a dotted line marks 1/a = 0.
#' * `"hanes"`: the Hanes plot, a/v against a.
#' * `"eadie"`: the Eadie-Hofstee plot, v against v/a. The envelope isn't drawn
#'   on this plot.
#' * `"direct"`: the direct linear plot, Michaelis-Menten only. Each data point
#'   (a, v) becomes a line through (-a, 0) and (0, v) in (Km, Vmax) space, and
#'   the lines meet near the best estimates. It needs `data.df` and ignores any
#'   curves.
#'
#' Not every transformation suits every model: see [BLOCKED_TRANSFORMATIONS].
#' Points that a transformation makes infinite (e.g. 1/0) are dropped with a
#' warning.
#'
#' **Colours.** Models with an extra variable (`I`, `B` or `R`) get a colour
#' and a legend entry per value. Other models are drawn in one colour without
#' a legend. If the palette has too few colours, ggplot2's default palette is
#' used instead.
#'
#' **Axes.** `x.units` and `y.units` are the units of the raw variables, and
#' the axis labels show the units of what is plotted (e.g. 1/mM on a
#' Lineweaver-Burk plot). For the binding models the y-axis shows 0 to 1
#' unless `y.range` is given.
#'
#' @param model The model's code, one of [VALID_MODELS], e.g. `"MM"`.
#' @param data.df Data to draw as points: a data frame with the model's
#'   variables as columns, e.g. from [simulate_data()] or your own data.
#'   Optional.
#' @param curve.df A curve to draw as lines, from [make_curve()]. Optional.
#' @param extra.curve A second curve, drawn in black on top. EKA2 uses it for
#'   the true curve of simulated data. Optional.
#' @param plot.transformation `"standard"` (default), `"log10"`,
#'   `"lineweaver"`, `"hanes"`, `"eadie"` or `"direct"`. See Details.
#' @param conf.int If `TRUE`, shade the parameter uncertainty envelope.
#'   `curve.df` must then come from `make_curve(..., conf.int = TRUE)`. Default
#'   `FALSE`.
#' @param x.label,y.label Axis labels to use instead of the defaults. Units are
#'   still added.
#' @param x.units,y.units The units of the raw x and y variables, e.g. `"mM"`
#'   and `"mM/min"`, added to the axis labels.
#' @param title A plot title to use instead of the default (the model's name
#'   and the transformation).
#' @param legend.label A legend title to use instead of the default (the extra
#'   variable's name).
#' @param palette A ColorBrewer palette from [PLOT_COLOUR_PALETTES] (default
#'   `"Set1"`).
#' @param hide.legend If `TRUE`, hide the legend. Default `FALSE`.
#' @param y.range The y-axis limits, a numeric vector of two values. Values
#'   outside them aren't drawn. Default `NULL` (automatic).
#' @param show.grid If `TRUE`, draw grid lines. Default `FALSE`.
#' @return A ggplot object.
#' @seealso [make_curve()] for curves, [make_residual_plot()] for residuals,
#'   [update_plot_curve()] to replace a plot's curve.
#' @author Haig Bishop
#' @importFrom ggplot2 .data
#' @examples
#' set.seed(1)
#' mm <- simulate_data("MM", list(Km = 0.5, Vmax = 2), x.min = 0.2, x.max = 5,
#'                     n.samples = 8)
#' curve <- make_curve("MM", list(Km = 0.5, Vmax = 2), x.min = 0.2, x.max = 5)
#' make_plot("MM", data.df = mm, curve.df = curve, x.units = "mM",
#'           y.units = "mM/min")
#'
#' # The same data and curve as a Lineweaver-Burk plot
#' make_plot("MM", data.df = mm, curve.df = curve,
#'           plot.transformation = "lineweaver")
#'
#' # The direct linear plot (data only)
#' make_plot("MM", data.df = mm, plot.transformation = "direct")
#'
#' # The result is a ggplot, so it can be changed further
#' make_plot("MM", data.df = mm, curve.df = curve) + ggplot2::theme_bw()
#' @export

make_plot <- function(model, data.df = NULL, curve.df = NULL, extra.curve = NULL, 
                      plot.transformation = "standard", conf.int = FALSE,
                      x.label = NULL, y.label = NULL,
                      x.units = NULL, y.units = NULL, title = NULL, 
                      legend.label = NULL, palette = "Set1", hide.legend = FALSE, 
                      y.range = NULL, show.grid = FALSE) {
    
    # Error Handling ================
    # Check if model is valid
    if (!model %in% VALID_MODELS) {
        stop("Invalid model. Please choose a valid model such as 'MM' or 'MMSI'.")
    }
    # Check if data.df is a dataframe if provided
    if (!is.null(data.df) && !is.data.frame(data.df)) {
        stop("data.df must be a dataframe.")
    }
    # Ensure data.df is not an empty dataframe (if provided)
    if (!is.null(data.df) && nrow(data.df) == 0) {
        stop("data.df must contain at least one row of data.")
    }
    # Check if curve.df is a dataframe if provided
    if (!is.null(curve.df) && !is.data.frame(curve.df)) {
        stop("curve.df must be a dataframe.")
    }
    # Ensure curve.df is not an empty dataframe (if provided)
    if (!is.null(curve.df) && nrow(curve.df) == 0) {
        stop("curve.df must contain at least one row of data.")
    }
    # Check if extra.curve is a dataframe if provided
    if (!is.null(extra.curve) && !is.data.frame(extra.curve)) {
        stop("extra.curve must be a dataframe.")
    }
    # Check if plot.transformation is valid
    if (!(plot.transformation %in% names(PLOT_TRANSFORMATIONS))) {
        stop("plot.transformation is invalid.")
    }
    # Ensure palette is valid
    if (!palette %in% PLOT_COLOUR_PALETTES) {
        stop("Invalid colour palette. Please choose a valid palette from: ", paste(PLOT_COLOUR_PALETTES, collapse = ", "))
    }
    # Ensure data is provided if it is a direct linear plot
    if (plot.transformation == "direct" && is.null(data.df)) {
        stop("For a direct linear plot, data.df must be provided.")
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
    # Check if show.grid is a boolean
    if (!is.logical(show.grid)) {
        stop("show.grid must be TRUE or FALSE.")
    }
    # Check if conf.int is a logical
    if (!is.logical(conf.int)) {
        stop("conf.int must be TRUE or FALSE.")
    }
    # Check if hide.legend is a logical
    if (!is.logical(hide.legend)) {
        stop("hide.legend must be TRUE or FALSE.")
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
    # If direct linear plot, there shouldn't be any curves
    if (plot.transformation == "direct") {
        if (!is.null(curve.df) || !is.null(extra.curve)) {
            warning("For a direct linear plot, curves are not used.")
            curve.df <- NULL
            extra.curve <- NULL
        }
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
    # ===============================
    
    
    # Initialise Default Plot ===================
    # Retrieve the default plot title and axis labels based on the model
    default.title <- PLOT_TITLES[[model]]
    default.x.axis <- AXIS_TITLES[[first.independent.var]]
    default.y.axis <- AXIS_TITLES[[dependent.var]]
    # Adjust title according to transformation
    transformation_text <- PLOT_TRANSFORMATION_TITLES[[plot.transformation]]
    plot.title <- paste(default.title, transformation_text)
    # If direct linear plot (special case)
    if (plot.transformation == "direct") {
        x.axis <- PLOT_TRANSFORMATION_X_AXIS_TITLES[[plot.transformation]]
        y.axis <- PLOT_TRANSFORMATION_Y_AXIS_TITLES[[plot.transformation]]
    } else {
        # Adjust x-axis title according to transformation
        transformation_text <- PLOT_TRANSFORMATION_X_AXIS_TITLES[[plot.transformation]]
        transformation_text <- gsub("X-AXIS", default.x.axis, transformation_text)
        transformation_text <- gsub("Y-AXIS", default.y.axis, transformation_text)
        x.axis <- paste(transformation_text, default.x.axis)
        # Adjust y-axis title according to transformation
        transformation_text <- PLOT_TRANSFORMATION_Y_AXIS_TITLES[[plot.transformation]]
        transformation_text <- gsub("X-AXIS", default.x.axis, transformation_text)
        transformation_text <- gsub("Y-AXIS", default.y.axis, transformation_text)
        y.axis <- paste(transformation_text, default.y.axis)
    }
    # Derive the units appropriate for the plotted (transformed) quantity. 
    # x.units and y.units describe the raw data variables. 
    # For transformed axes, the displayed units must reflect the mathematical operation applied.
    effective.x.units <- x.units
    effective.y.units <- y.units
    if (plot.transformation == "lineweaver") {
        # Both axes are reciprocals: 1/x and 1/y
        if (!is.null(x.units) && x.units != "") effective.x.units <- paste0("1/", divisor_unit(x.units))
        if (!is.null(y.units) && y.units != "") effective.y.units <- paste0("1/", divisor_unit(y.units))
    } else if (plot.transformation == "hanes") {
        # y-axis is [A]/V; x-axis is unchanged
        if (!is.null(x.units) && x.units != "" && !is.null(y.units) && y.units != "") {
            effective.y.units <- paste0(x.units, "/", divisor_unit(y.units))
        } else if (!is.null(x.units) && x.units != "") {
            effective.y.units <- x.units
        } else {
            effective.y.units <- NULL
        }
    } else if (plot.transformation == "eadie") {
        # x-axis is V/[A]; y-axis is unchanged
        if (!is.null(y.units) && y.units != "" && !is.null(x.units) && x.units != "") {
            effective.x.units <- paste0(y.units, "/", divisor_unit(x.units))
        } else if (!is.null(y.units) && y.units != "") {
            effective.x.units <- y.units
        } else {
            effective.x.units <- NULL
        }
    } else if (plot.transformation == "log10") {
        # log10 transforms the x-axis to a dimensionless quantity
        effective.x.units <- NULL
    }
    # Append units to axis labels if x.units or y.units are provided
    if (!is.null(effective.x.units) && effective.x.units != "") {
        x.axis <- paste0(x.axis, " (", effective.x.units, ")")
    }
    if (!is.null(effective.y.units) && effective.y.units != "") {
        y.axis <- paste0(y.axis, " (", effective.y.units, ")")
    }
    
    # Make default plot
    extra.independent.var <- if (length(model.vars) > 2) model.vars[3] else NULL
    legend_name <- if (!is.null(legend.label)) legend.label else if (!is.null(extra.independent.var)) AXIS_TITLES[[extra.independent.var]] else "Legend"
    plot <- ggplot2::ggplot() + 
        ggplot2::xlab(x.axis) +
        ggplot2::ylab(y.axis) +
        ggplot2::ggtitle(plot.title) +
        ggplot2::labs(color = legend_name) +
        ggthemes::theme_few() +
        ggplot2::theme(
            panel.grid = if(show.grid) 
                ggplot2::element_line(color = "grey69", linewidth = 0.5) 
            else 
                ggplot2::element_blank()
        )
    
    # Get the valid domain of the dependent variable
    dependent.var.domain <- MODEL_DEPENDENT_VAR_DOMAINS[[model]]
    # If it doesn't include infinity, zoom without dropping out-of-domain points
    if (!Inf %in% dependent.var.domain && is.null(y.range)) {
        plot <- plot + ggplot2::coord_cartesian(ylim = dependent.var.domain)
    }
    # ===============================
    
    
    # Transform the data and/or curves ===============
    transformation <- PLOT_TRANSFORMATIONS[[plot.transformation]]
    if (!is.null(data.df)) {
        data.df = transformation(data.df, x.name = first.independent.var, y.name = dependent.var)
        data.df <- drop_nonfinite_xy(data.df, first.independent.var, dependent.var, plot.transformation)
    }
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
    
    
    # Add Extra Plot Features ===================
    # Count the number of colour series (unique levels of the extra independent variable)
    n.series <- 1
    if (length(model.vars) > 2) {
        extra.var.name <- model.vars[3]
        all.levels <- c()
        if (!is.null(data.df)) all.levels <- c(all.levels, unique(data.df[[extra.var.name]]))
        if (!is.null(curve.df)) all.levels <- c(all.levels, unique(curve.df[[extra.var.name]]))
        n.series <- max(1, length(unique(all.levels)))
    }
    # Add colour palette (falls back to hue scale if palette has too few colours)
    plot <- plot + colour_scale(palette, n.series)
    
    # If this is a lineweaver-burk plot add a line at the y-axis
    if (plot.transformation == "lineweaver") {
        plot <- plot + ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "black")
    }
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
    # =================================
    
    
    # Confidence Intervals ============
    if (conf.int) {
        # If we are using 2 independent variables
        if (!is.null(extra.independent.variable)) {
            # Sort the dataframe by the extra independent variable
            curve.df <- curve.df[order(curve.df[[extra.independent.variable]]), ]
            # Split the dataframe into list of multiple df - one per value of the extra independent variable
            curve.dfs <- split(curve.df, curve.df[[extra.independent.variable]])
        } else {
            # "Split" the dataframe into a list of one df - because only one set of points
            curve.dfs <- list(curve.df)
        }
        # Loop through each data df
        for (i in seq_along(curve.dfs)) {
            # Add a parameter uncertainty envelope ribbon to the plot
            plot <- plot + ggplot2::geom_ribbon(
                data = curve.dfs[[i]], 
                ribbon_aes(first.independent.var, dependent.var, extra.independent.variable), 
                alpha=0.3, inherit.aes = FALSE,
                show.legend = FALSE
            )
        }
        plot <- plot + colour_scale(palette, n.series, aesthetic = "fill")
        # Now we can for sure remove upper and lower curves to prevent issues later in the function
        curve.df <- curve.df[, !grepl("\\.lb$|\\.ub$", names(curve.df))]
    }
    # =================================
    
    # If direct linear plot 
    if (plot.transformation == "direct") {
        # If we have data
        if (!is.null(data.df)) {
            # Calculate all lines (only from y=0 to x=0, for now)
            lines.df <- data.frame(x1 = numeric(), y1 = numeric(), x2 = numeric(), y2 = numeric())
            # Loop through every observation (row) in data.df
            for (i in 1:nrow(data.df)) {
                a <- data.df$A[i]
                v <- data.df$V[i]
                if (a == 0) {
                    next
                }
                x1 <- -1 * a
                y1 <- 0
                x2 <- 0
                y2 <- v
                # Add the line to lines.df
                lines.df <- rbind(lines.df, data.frame(x1 = x1, y1 = y1, x2 = x2, y2 = y2))
                
            }
            if (nrow(lines.df) > 0) {
                # Compute maximum x and y values
                max.x <- max(data.df$A)
                y_multiplier <- 1.5     # Expand y-axis range by arbitrary amount
                max.y <- max(data.df$V) * y_multiplier  
                
                # Extend each line to max values, while keeping their slopes the same
                for (i in 1:nrow(lines.df)) {
                    # Calculate the slope of the line
                    slope <- (lines.df$y2[i] - lines.df$y1[i]) / (lines.df$x2[i] - lines.df$x1[i])
                    y_intercept <- lines.df$y1[i] - slope * lines.df$x1[i]
                    
                    # Calculate intersection with max.x
                    y_at_max_x <- slope * max.x + y_intercept
                    
                    # Calculate intersection with max.y
                    x_at_max_y <- (max.y - y_intercept) / slope
                    
                    # Determine which points to use as endpoints
                    if (0 <= x_at_max_y && x_at_max_y <= max.x) {
                        # x_at_max_y is within the bounds
                        lines.df$x2[i] <- x_at_max_y
                        lines.df$y2[i] <- max.y
                    } else {
                        # y_at_max_x is within the bounds
                        lines.df$x2[i] <- max.x
                        lines.df$y2[i] <- y_at_max_x
                    }
                }
                # Draw the lines
                plot <- plot + 
                    ggplot2::geom_segment(data = lines.df, 
                                       ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2, colour = "This is the only colour"), 
                                       inherit.aes = FALSE)
            }
        }
        # Draw the y-axis
        plot <- plot + 
            ggplot2::geom_vline(
                xintercept = 0, 
                color = "black"
            )
        
    # =================================
    } else {
        
        # If data was given
        if (!is.null(data.df)) {
            # If we are using 2 independent variables
            if (!is.null(extra.independent.variable)) {
                # Sort the dataframe by the extra independent variable
                data.df <- data.df[order(data.df[[extra.independent.variable]]), ]
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
                                        colour_aes(first.independent.var, dependent.var, extra.independent.variable), 
                                        inherit.aes = FALSE)
            }
        }
        # If a curve(s) was given
        if (!is.null(curve.df)) {
            # If we are using 2 independent variables
            if (!is.null(extra.independent.variable)) {
                # Sort the dataframe by the extra independent variable
                curve.df <- curve.df[order(curve.df[[extra.independent.variable]]), ]
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
                    # Calculate the y-intercept using the formula c = y - mx
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
                
                # Draw the curve
                plot <- plot + 
                    ggplot2::geom_path(data = this.curve.df, 
                                       colour_aes(first.independent.var, dependent.var, extra.independent.variable), 
                                       inherit.aes = FALSE)
            }
        }
        # If an extra curve(s) was given
        if (!is.null(extra.curve)) {
            # If we are using 2 independent variables
            if (!is.null(extra.independent.variable)) {
                # Sort the dataframe by the extra independent variable
                extra.curve <- extra.curve[order(extra.curve[[extra.independent.variable]]), ]
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
                # Draw the curve
                plot <- plot + 
                    ggplot2::geom_path(data = this.curve.df, 
                                       ggplot2::aes(x = .data[[first.independent.var]], y = .data[[dependent.var]]), 
                                       inherit.aes = FALSE)
            }
        }
    }
    
    # If an x-axis label was given
    if (!is.null(x.label)) {
        # Append units to axis labels if x.units is provided
        if (!is.null(effective.x.units) && effective.x.units != "") {
            x.label <- paste0(x.label, " (", effective.x.units, ")")
        }
        # Add the label
        plot <- plot + ggplot2::xlab(x.label)
    }
    # If an y-axis label was given
    if (!is.null(y.label)) {
        # Append units to axis labels if y.units is provided
        if (!is.null(effective.y.units) && effective.y.units != "") {
            y.label <- paste0(y.label, " (", effective.y.units, ")")
        }
        # Add the label
        plot <- plot + ggplot2::ylab(y.label)
    }
    # If a title was given
    if (!is.null(title)) {
        # Add the title
        plot <- plot + ggplot2::ggtitle(title)
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

