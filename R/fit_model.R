## Function file for enzKinet2 package

#' Fit Model
#'
#' @author Haig Bishop
#' 28/06/2024
#'
#' Fits the model to the data. Returns the fitted.params (NULL if could not fit)
#' @param model The chosen model ("MM", "MMSI", etc.)
#' @param data.df The data to fit the model to
#' @param start.params The unfitted parameters for the model (e.g. Km, Vmax)
#' @param fit.method The algorithm or calculation used to fit the model.
#' @param locked.params A vector of parameter names to lock (e.g., c("Km"))
#' @param add.minor.noise Boolean to add a tiny amount of noise to the data before fitting.
#' @param override.data.point.check Boolean to override num data points checks.
#' @param get.conf.int Boolean to get extra parameter values for upper and lower bounds.
#' @param conf.level Float for confidence level of confidence interval.
#' @param get.stats Boolean to get statistics of fit
#' @return fitted.params
#' 
#' @export

fit_model <- function(model, data.df, start.params = NULL, fit.method = "nls", locked.params = NULL, 
                      add.minor.noise = FALSE, override.data.point.check = FALSE,
                      get.conf.int = FALSE, conf.level = 0.95, get.stats = FALSE) {
    # Error Handling ================
    # Check if model is valid
    if (!model %in% VALID_MODELS) {
        stop("Invalid model. Please choose a valid model such as 'MM'.")
    }
    # Check if start.params is a list if fit.method is NLS
    if (!is.list(start.params) && fit.method == "nls") {
        stop("start.params must be a list when using nonlinear least squares.")
    }
    # Check if locked.params is a vector
    if (!is.null(locked.params) && !is.vector(locked.params)) {
        stop("locked.params must be a vector.")
    }
    # Check if data.df is a dataframe
    if (!is.data.frame(data.df)) {
        stop("data.df must be a dataframe.")
    }
    # Check if data.df is not empty
    if (nrow(data.df) == 0) {
        stop("data.df must not be empty.")
    }
    # Check if fit.method is valid
    if (!is.null(fit.method) && !fit.method %in% names(FITTING_METHODS)) {
        stop("Invalid fit method. Please choose a valid method such as 'nls', 'recursive', 'ss_calc' or 'nonparametric.")
    }
    # Check if model is valid for the fit.method
    if (!is.null(fit.method) && !model %in% FITTING_METHODS[[fit.method]]) {
        stop(paste("The model", model, "is not valid for the fit method", fit.method, "."))
    }
    # Check if fit.method is not "nls" and locked.params is TRUE
    if (fit.method != "nls" && !is.null(locked.params)) {
        stop("Locked parameters can only be used with the 'nls' fit method.")
    }
    # Check if using confidence intervals without "nls"
    if (fit.method != "nls" && get.conf.int) {
        stop("Confidence intervals can only be derived with the 'nls' fit method.")
    }
    # Check if getting stats without "nls"
    if (fit.method != "nls" && get.stats) {
        stop("Statistics can only be derived with the 'nls' fit method.")
    }
    # Check if using confidence level is okay
    if (get.conf.int && !(is.numeric(conf.level) && conf.level > 0 && conf.level < 1)) {
        stop("Confidence level must be greater than 0 and less than 1.")
    }
    # Check number of data points
    num_data_points <- nrow(data.df)
    if (!override.data.point.check) {
        if (num_data_points < 3) {
            stop("Less than 3 data points is unlikely insufficient to fit the model. Overide this by setting override.data.point.check to TRUE.")
        }
    }
    # ===============================
    
    
    # Model-specific error handling =======
    # Grab model-specific values
    model.vars <- MODEL_VARIABLES[[model]]
    full.model.name <- PLOT_TITLES[[model]]
    model.vars.string <- MODEL_VARIABLE_STRINGS[[model]]
    # Check if data.df has the necessary columns (variables)
    if (!all(model.vars %in% colnames(data.df))) {
            stop(paste("For the", full.model.name, "model, data.df must contain columns (variables) named", model.vars.string, "."))
    }
    # ===============================
    
    
    # Add minor noise =========================
    if (add.minor.noise) {
        # For reproducibility
        set.seed(24)
        noise <- rnorm(n = nrow(data.df), mean = 0, sd = 1e-6)
        data.df <- data.df + noise
    }
    # ===============================
    
    
    # Set up model ready to fit ================
    # Extract the variables according to the chosen model
    param.names <- MODEL_PARAMETERS[[model]]
    full.model.name <- PLOT_TITLES[[model]]
    model.params.string <- MODEL_PARAMETER_STRINGS[[model]]
    # Initialise empty statistics
    statistics <- NULL
    
    # If nonlinear least squares
    if (fit.method == "nls") {
        # Check if required parameters are present
        if (!all(param.names %in% names(start.params))) {
            stop(paste("For the", full.model.name, "model, alongwith nonlinear least squares, start.params must include", model.params.string, "."))
        }
        # Check if the parameters are numeric and greater than 0
        for (param in param.names) {
            if (!is.numeric(start.params[[param]])) {
                stop(paste(param, "must be a numeric value."))
            }
            if (start.params[[param]] <= 0) {
                stop(paste(param, "must be greater than 0."))
            }
        }
        # Ensure all values of locked.params are in param.names
        if (!is.null(locked.params) && !all(locked.params %in% param.names)) {
            stop("All values of locked.params must be from:", model.params.string, ".")
        }
        # ===============================
        
        # Define model
        model.formula <- MODEL_FORMULAE[[model]]
        # Replace variables in model.formula with values from locked.params
        if (!is.null(locked.params)) {
            for (locked.param in locked.params) {
                if (locked.param %in% param.names) {
                    param.value <- start.params[[locked.param]]
                    model.formula <- as.formula(gsub(as.character(locked.param), param.value, deparse(model.formula)))
                }
            }
        }
        # Define some NLS control parameters
        ctrl <- nls.control(maxiter = 1e4, tol = 1e-5)
        # Extract only the needed starting parameters
        fit.start.params <- as.list(start.params[param.names])
        if (!is.null(locked.params)) {
            fit.start.params <- fit.start.params[!names(fit.start.params) %in% locked.params]
        }
        # Fit model =================
        fitted.params <- NULL
        tryCatch({
            # Fit with NLS
            fit <- nls(model.formula, data = data.df, start = fit.start.params, control = ctrl)
            fitted.params <- as.list(coef(fit))
            names(fitted.params) <- names(coef(fit))
            # Get confidence intervals 
            if (get.conf.int) {
                confidence.intervals = nlstools::confint2(fit, level = conf.level)
                # Unpack into fitted params (e.g. Km.lb and Km.ub)
                for (param in rownames(confidence.intervals)) {
                    lower_bound <- confidence.intervals[param, 1]
                    # Ensure above 0
                    if (lower_bound <= 0) {lower_bound <- 1e-5}
                    upper_bound <- confidence.intervals[param, 2]
                    fitted.params[[paste0(param, ".lb")]] <- lower_bound
                    fitted.params[[paste0(param, ".ub")]] <- upper_bound
                }
            }
            if (get.stats) {
                # Extract the standard errors for each parameter
                standard.errors <- unname(summary(fit)$coefficients[, 2])
                # Extract the parameter names
                parameter.names <- rownames(summary(fit)$coefficients)
                # Initialize an empty list for the statistics
                statistics <- list()
                # Add the standard errors to the statistics list with ".se" extension
                for (i in seq_along(parameter.names)) {
                    statistics[[paste0(parameter.names[i], ".se")]] <- standard.errors[i]
                }
                # Calculate additional statistics
                R2 <- modelr::rsquare(fit, data.df)
                RMSE <- modelr::rmse(fit, data.df)
                MAE <- modelr::mae(fit, data.df)
                glance <- broom::glance(fit)
                # Add these statistics to the list
                statistics$R2 <- R2
                statistics$RMSE <- RMSE
                statistics$MAE <- MAE
                statistics$AIC <- glance$AIC
                statistics$BIC <- glance$BIC
                statistics$logLik <- glance$logLik
            }
            
        }, error = function(e) {
            message("Model fitting failed: ", e$message)
            message("Failiure to fit could be explained by noiseless data, poor starting parameters, over parametrisation , etc..")
            fitted.params <- NULL
        })
        # Return locked.params ===================
        # Add locked.params to fitted.params
        if (!is.null(fitted.params) && !is.null(locked.params)) {
            for (locked.param in locked.params) {
                fitted.params[locked.param] <- start.params[[locked.param]]
            }
        }
    }
    # ===============================
    
    
    # If recursive method
    if (fit.method == "recursive") {
        # Get the MM function
        MM_function <- MODEL_FUNCTIONS[[model]]
        # Define function to calculate the parameters as estimate
        calculate.params <- function (data) {
            # Define a small epsilon value
            eps <- 1e-10
            # Transform the vectors as needed
            v3 <- data$V^3
            v4 <- data$V^4
            a1 <- data$A^1
            a2 <- data$A^2
            # Calculate needed sums
            sum_v4_by_a2 <- sum(v4/(a2+eps))
            sum_v4_by_a1 <- sum(v4/(a1+eps))
            sum_v4 <- sum(v4)
            sum_v3_by_a1 <- sum(v3/(a1+eps))
            sum_v3 <- sum(v3)
            # Use this equation to calculate Km estimate
            est.Km <- (sum_v4*sum_v3_by_a1 - sum_v4_by_a1*sum_v3) / 
                (sum_v4_by_a2*sum_v3 - sum_v4_by_a1*sum_v3_by_a1 + eps)
            # Use this equation to calculate Vmax estimate
            est.Vmax <- (sum_v4_by_a2*sum_v4 - sum_v4_by_a1^2) / 
                (sum_v4_by_a2*sum_v3 - sum_v4_by_a1*sum_v3_by_a1 + eps)
            # Return estimates
            est.params <- list("Km" = est.Km, "Vmax" = est.Vmax)
            return (est.params)
        }
        # Define function to calculate the parameters recursively until convergence
        recursive.fit <- function (data, params=NULL, max.iter=1e3, max.tol=1e-6) {
            # If reached max iterations
            if (max.iter < 1) {
                # Print warning
                warning("Reached maximum iterations without convergence.")
                return (params)
            # If starting with no params
            } else if (is.null(params)) {
                # Calculate approximate Km and Vmax using data
                est.params <- calculate.params(data)
                # Use approximate Km and Vmax with A's to calculate v's
                est.data <- MM_function(est.params, data$A, NULL)
                # Recursively call function using estimate rates
                return(recursive.fit(est.data, est.params, max.iter=max.iter-1))
                
            } else {
                # Calculate approximate Km and Vmax using data
                est.params <- calculate.params(data)
                # If difference between estimated params and previous params is under tolerance
                if ((abs(est.params$Km - params$Km) < max.tol) && (abs(est.params$Vmax - params$Vmax) < max.tol)) {
                    # Converged
                    return(params)
                } else {
                    # Use approximate Km and Vmax with A's to calculate v's
                    est.data <- MM_function(est.params, data$A, NULL)
                    # Recursively call function using estimate rates
                    return(recursive.fit(est.data, est.params, max.iter=max.iter-1))
                }
            }
        }
        # Use function to estimate parameters
        fitted.params <- recursive.fit(data.df)
    }
    # ===============================
    
    
    
    # If sum of squares calculation method
    if (fit.method == "ss_calc") {
        # Define a small epsilon value
        eps <- 1e-10
        # Transform the vectors as needed
        v1 <- data.df$V^1
        v2 <- data.df$V^2
        a1 <- data.df$A^1
        a2 <- data.df$A^2
        # Calculate needed sums
        sum_v2_by_a2 <- sum(v2/(a2+eps))
        sum_v2_by_a1 <- sum(v2/(a1+eps))
        sum_v1_by_a1 <- sum(v1/(a1+eps))
        sum_v2 <- sum(v2)
        sum_v1 <- sum(v1)
        # Use this equation to calculate Km estimate
        est.Km <- (sum_v2 * sum_v1_by_a1 - sum_v2_by_a1 * sum_v1) / 
            (sum_v2_by_a2 * sum_v1 - sum_v2_by_a1 * sum_v1_by_a1 + eps)
        # Use this equation to calculate Vmax estimate
        est.Vmax <- (sum_v2_by_a2 * sum_v2 - sum_v2_by_a1^2) / 
            (sum_v2_by_a2 * sum_v1 - sum_v2_by_a1 * sum_v1_by_a1 + eps)
        # Return estimates
        fitted.params <- list("Km" = est.Km, "Vmax" = est.Vmax)
    }
    # ===============================
    
    
    
    # If non-parametric method
    if (fit.method == "nonparametric") {
        # Define a small epsilon value
        eps <- 1e-10
        # Initialise empty lists
        all.est.Vmax <- c()
        all.est.Km <- c()
        # Loop through every pair of observations
        for (i in 1:nrow(data.df)) {
            for (j in 1:nrow(data.df)) {
                # If these are nonduplicate
                if (data.df$A[i] != data.df$A[j]) {
                    # Extract values Ai, Vi, Aj, Vj
                    Ai <- data.df$A[i]
                    Vi <- data.df$V[i]
                    Aj <- data.df$A[j]
                    Vj <- data.df$V[j]
                    # Compute Km and Vmax values for this pair of observations
                    est.Vmax <- (Ai - Aj) / ((Ai / (Vi + eps)) - (Aj / (Vj + eps) + eps))
                    est.Km <- (Vj - Vi) / ((Vi / (Ai + eps)) - (Vj / (Aj + eps) + eps))
                    # Set any pair of estimated values in the 3rd quadrant to large positive values
                    # (see page 431 Fundamentals of Enzyme Kinetics 4th Edition)
                    if (est.Vmax < 0 && est.Km < 0) {
                        # Large values won't affect median too much
                        est.Vmax <- 1e20
                        est.Km <- 1e20
                    }
                    # Add to the lists
                    all.est.Vmax <- c(all.est.Vmax, est.Vmax)
                    all.est.Km <- c(all.est.Km, est.Km)
                }
            }
        }
        
        # Find medians of both (if even number of obs, take mean of centre two)
        median.est.Vmax <- median(all.est.Vmax)
        median.est.Km <- median(all.est.Km)
        # Return estimates
        fitted.params <- list("Km" = median.est.Km, "Vmax" = median.est.Vmax)
    }
    # ===============================
    
    # If any estimates are out of valid range
    if (!is.null(fitted.params) && (any(fitted.params <= 0) || any(fitted.params > 1e19))) {
        fitted.params <- NULL
        message("Model fitting returned parameter values out of valid range.")
    } 
    
    if (!get.stats) {
        # Return the fitted params (NULL if model could not fit)
        return(fitted.params)
    } else {
        # Also return stats
        return(list(fitted_params = fitted.params, statistics = statistics))
    }
}
