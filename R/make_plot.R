## Function file for enzKinet2 package

#' Make Plot
#'
#' @author Haig Bishop
#' 07/09/2024
#'
#' Generates a plot for an enzyme kinetics model.
#' @param model The model we are plotting ("MM", "CI", etc.).
#' @param data.df Data to be plotted as dots.
#' @param curve.df Curve to be plotted as line.
#' @param extra.curve An extra curve to be plotted on top of the other (black).
#' @param plot.transformation Transformation to apply to data before plotting (e.g., "standard", "lineweaver", "direct").
#' @param conf.int Whether or not to plot confidence intervals.
#' @param x.label Custom x-axis label.
#' @param y.label Custom y-axis label.
#' @param x.units Units for the x-axis, such as "mM" or "µM". Defaults to NULL (no units).
#' @param y.units Units for the y-axis, such as "µmol/s" or "U/mL". Defaults to NULL (no units).
#' @param title Custom plot title.
#' @param legend.label Custom legend label.
#' @param palette Custom plot colour palette (e.g., "Set1", "Set2", etc.).
#' @param hide.legend Boolean to hide the plot legend.
#' @return A ggplot object representing the enzyme kinetics plot.
#' 
#' @export

make_plot <- function(model, data.df = NULL, curve.df = NULL, extra.curve = NULL, 
                      plot.transformation = "standard", conf.int = FALSE,
                      x.label = NULL, y.label = NULL,
                      x.units = NULL, y.units = NULL, title = NULL, 
                      legend.label = NULL, palette = "Set1", hide.legend = FALSE) {
    
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
    # Ensure curve.df is not an empty dataframe (if provided)
    if (!is.null(curve.df) && nrow(curve.df) == 0) {
        stop("curve.df must contain at least one row of data.")
    }
    # Check if curve.df is a dataframe if provided
    if (!is.null(curve.df) && !is.data.frame(curve.df)) {
        stop("curve.df must be a dataframe.")
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
        stop(paste("Invalid colour palette. Please choose a valid palette from:", PLOT_COLOUR_PALETTES))
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
    # If direct linear plot, there shouldn't be any curves
    if (plot.transformation == "direct") {
        if (!is.null(curve.df) || !is.null(extra.curve)) {
            warning("For a direct linear plot, curves are not used.")
            curve.df <- NULL
            extra.curve <- NULL
        }
    }
    # Check if confidence interval curves were provided (if used)
    if (conf.int && is.null(curve.df)) {
        stop("Confidence intervals requested, but curve.df is missing.")
    }
    if (conf.int && !all(CONFIDENCE_INTERVAL_BOUNDING_VARIABLES[[model]] %in% colnames(curve.df))) {
        stop("Confidence interval curves (e.g. 'V.lb', 'V.ub') not provided.")
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
    # Append units to axis labels if x.units or y.units are provided
    if (!is.null(x.units) && x.units != "") {
        x.axis <- paste0(x.axis, " (", x.units, ")")
    }
    if (!is.null(y.units) && y.units != "") {
        y.axis <- paste0(y.axis, " (", y.units, ")")
    }
    
    # Make default plot
    extra.independent.var <- if (length(model.vars) > 2) model.vars[3] else NULL
    legend_name <- if (!is.null(legend.label)) legend.label else if (!is.null(extra.independent.var)) AXIS_TITLES[[extra.independent.var]] else "Legend"
    plot <- ggplot2::ggplot() + 
        ggplot2::xlab(sprintf(x.axis)) +
        ggplot2::ylab(sprintf(y.axis)) +
        ggplot2::ggtitle(plot.title) +
        ggplot2::labs(color = legend_name) +
        ggthemes::theme_few()
    
    # Get the valid domain of the dependent variable
    dependent.var.domain <- MODEL_DEPENDENT_VAR_DOMAINS[[model]]
    # If it doesn't include infinity
    if (!Inf %in% dependent.var.domain) {
        # Set y-axis range to it (e.g. between 0 and 1)
        plot <- plot + ggplot2::scale_y_continuous(limits = dependent.var.domain)
    }
    # ===============================
    
    
    # Transform the data and/or curves ===============
    transformation <- PLOT_TRANSFORMATIONS[[plot.transformation]]
    if (!is.null(data.df)) {
        data.df = transformation(data.df, x.name = first.independent.var, y.name = dependent.var)
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
            
            # Reintegrate transformed lower and upper bound curves into curve.df
            curve.df <- cbind(curve.df[, !grepl("\\.lb$|\\.ub$", names(curve.df))], lb.curve.df, ub.curve.df)
        }
    }
    if (!is.null(extra.curve)) {
        extra.curve = transformation(extra.curve, x.name = first.independent.var, y.name = dependent.var)
    }
    # ===============================
    
    
    # Add Extra Plot Features ===================
    # Add colour palette
    plot <- plot + ggplot2::scale_color_brewer(palette = palette)
    fill_palette <- ggplot2::scale_fill_brewer(palette = palette)
    
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
            # Add a confidence interval ribbon to the plot
            plot <- plot + ggplot2::geom_ribbon(
                data = curve.dfs[[i]], 
                ggplot2::aes_string(
                    x = first.independent.var, 
                    ymin = paste0(dependent.var, ".lb"), 
                    ymax = paste0(dependent.var, ".ub"),
                    fill = colours
                ), 
                alpha=0.3, inherit.aes = FALSE,
                show.legend = FALSE
            ) + fill_palette
        }
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
                # Calculate points for the lines
                a <- data.df$A[i]
                v <- data.df$V[i]
                x1 <- -1 * a
                y1 <- 0
                x2 <- 0
                y2 <- v
                # Add the line to lines.df
                lines.df <- rbind(lines.df, data.frame(x1 = x1, y1 = y1, x2 = x2, y2 = y2))
                
            }
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
                                   ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2, color = colours), 
                                   inherit.aes = FALSE)
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
                                        ggplot2::aes_string(x = first.independent.var, y = dependent.var, color = colours), 
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
                                       ggplot2::aes_string(x = first.independent.var, y = dependent.var, color = colours), 
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
                                       ggplot2::aes_string(x = first.independent.var, y = dependent.var, color = NULL), 
                                       inherit.aes = FALSE)
            }
        }
    }
    
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
    # ===============================
    
    return(plot)
}
