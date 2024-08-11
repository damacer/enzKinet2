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
#' @param extra.curve An extra curve to be displayed on top of the other
#' @param plot.transformation Transformation to give the data.
#' @param x.label Custom x-axis label.
#' @param y.label Custom y-axis label.
#' @param title Custom plot title.
#' @return plot
#' 
#' @export

make_plot <- function(model, data.df = NULL, curve.df = NULL, extra.curve = NULL, 
                      plot.transformation = "standard", 
                      x.label = NULL, y.label = NULL, title = NULL) {
    
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
    # Check is plot.transformation is valid
    if (!(plot.transformation %in% names(PLOT_TRANSFORMATIONS))) {
        stop("plot.transformation is invalid.")
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
        }
    }
    # ===============================
    
    
    # Initialise Default Plot ===================
    
    # Grab the default title from dictionary
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
    
    # Make default plot
    plot <- ggplot2::ggplot() + 
        ggplot2::xlab(sprintf(x.axis)) +
        ggplot2::ylab(y.axis) +
        ggplot2::ggtitle(plot.title) +
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
    
    
    # Transform the data and/or curve ===============
    transformation <- PLOT_TRANSFORMATIONS[[plot.transformation]]
    if (!is.null(data.df)) {
        data.df = transformation(data.df, x.name = first.independent.var, y.name = dependent.var)
    }
    if (!is.null(curve.df)) {
        curve.df = transformation(curve.df, x.name = first.independent.var, y.name = dependent.var)
    }
    if (!is.null(extra.curve)) {
        extra.curve = transformation(extra.curve, x.name = first.independent.var, y.name = dependent.var)
    }
    # ===============================
    
    
    
    # Add Extra Plot Features ===================
    # Add colour pallette
    plot <- plot + ggplot2::scale_color_brewer(palette = "Set1")
    
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
            max.y <- max(data.df$V) * 1.5
            
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
        # Add the label
        plot <- plot + ggplot2::xlab(sprintf(x.label))
    }
    # If an y-axis label was given
    if (!is.null(y.label)) {
        # Add the label
        plot <- plot + ggplot2::ylab(sprintf(y.label))
    }
    # If a title was given
    if (!is.null(title)) {
        # Add the title
        plot <- plot + ggplot2::ggtitle(sprintf(title))
    }
    # ===============================
    
    
    return(plot)
}
