## Function file for enzKinet2 package

#' Fit Model
#'
#' Fits a model to the given data. Returns a list with fitted.params
#' (NULL if fitting failed), statistics, and error. Note that error can be a
#' CI-only problem with a valid fit (fitted.params still populated, without .lb/.ub bounds).
#'
#' @author Haig Bishop
#' @param model The model to fit the data ("MM", "CI", etc.)
#' @param data.df The data to fit the model to.
#' @param start.params The starting parameter values for the model (e.g. Km, Vmax).
#' @param fit.method The algorithm or calculation used to fit the model ("nls", "recursive", "ss.calc", and "nonparametric").
#' @param locked.params A vector of parameter names to lock while fitting.
#' @param add.minor.noise Add a negligible amount of noise to the data before fitting.
#' @param override.data.point.check Override low number of data points error/warnings.
#' @param get.conf.int Get extra parameter values for upper and lower bounds.
#' @param conf.level Confidence level value of confidence interval.
#' @param conf.method Confidence interval method ("profile" or "asymptotic").
#' @param get.stats Get statistics of the fit (returned alongside fitted.params).
#' @return A list containing fitted.params, statistics, and error.
#'   Note that error can be a CI-only problem with a valid fit.
#' @importFrom stats coef confint median nls nls.control rnorm
#'
#' @export

fit_model <- function(model, data.df, start.params = NULL, fit.method = "nls", locked.params = NULL,
                      add.minor.noise = FALSE, override.data.point.check = FALSE,
                      get.conf.int = FALSE, conf.level = 0.95, conf.method = "profile", get.stats = FALSE) {
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
    # Check if add.minor.noise is a logical
    if (!is.logical(add.minor.noise)) {
        stop("add.minor.noise must be TRUE or FALSE.")
    }
    # Check if override.data.point.check is a logical
    if (!is.logical(override.data.point.check)) {
        stop("override.data.point.check must be TRUE or FALSE.")
    }
    # Check if get.conf.int is a logical
    if (!is.logical(get.conf.int)) {
        stop("get.conf.int must be TRUE or FALSE.")
    }
    # Check if get.stats is a logical
    if (!is.logical(get.stats)) {
        stop("get.stats must be TRUE or FALSE.")
    }
    # Check if fit.method is valid
    if (!is.null(fit.method) && !fit.method %in% names(FITTING_METHODS)) {
        stop("Invalid fit method. Please choose a valid method such as 'nls', 'recursive', 'ss.calc' or 'nonparametric.")
    }
    # Check if model is valid for the fit.method
    if (!is.null(fit.method) && !model %in% FITTING_METHODS[[fit.method]]) {
        stop(paste("The model", model, "is not valid for the fit method", fit.method, "."))
    }
    # Check if fit.method is not "nls" and locked.params is TRUE
    if (fit.method != "nls" && !is.null(locked.params)) {
        stop("Locked parameters are only supported for the 'nls' fit method.")
    }
    # Check if using confidence intervals without "nls"
    if (fit.method != "nls" && get.conf.int) {
        stop("Confidence intervals can only be derived with the 'nls' fit method.")
    }
    # Check if getting stats without "nls"
    if (fit.method != "nls" && get.stats) {
        stop("Statistics can only be derived with the 'nls' fit method.")
    }
    # Check if using confidence level is valid
    if (get.conf.int && !(is.numeric(conf.level) && conf.level > 0 && conf.level < 1)) {
        stop("Confidence level must be a numeric value between 0 and 1 (exclusive).")
    }
    # Check if confidence interval method is valid
    if (get.conf.int && !(is.character(conf.method) && length(conf.method) == 1 && conf.method %in% c("profile", "asymptotic"))) {
        stop("conf.method must be either 'profile' or 'asymptotic'.")
    }
    # Check number of data points against free parameters
    num_data_points <- nrow(data.df)
    n.free <- length(MODEL_PARAMETERS[[model]])
    if (fit.method == "nls" && !is.null(locked.params)) {
        n.free <- n.free - sum(locked.params %in% MODEL_PARAMETERS[[model]])
    }
    if (!override.data.point.check) {
        min.points <- n.free + 1
        if (num_data_points < min.points) {
            stop("This model has ", n.free, " free parameter(s), so at least ", min.points, " data points are needed to fit. Override this by setting override.data.point.check to TRUE.")
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
    # Check if required columns are numeric
    for (var in model.vars) {
        if (!is.numeric(data.df[[var]])) {
            stop(paste("Column", var, "in data.df must be numeric."))
        }
    }
    # Check for NA values in required columns
    if (anyNA(data.df[, model.vars, drop = FALSE])) {
        stop("data.df must not contain NA values in the required columns.")
    }
    # QBK divides by R, so R must be strictly positive
    if (model == "QBK" && any(data.df$R <= 0)) {
        stop("For the Quadratic Binding Kinetics model, R values must be greater than 0.")
    }
    # OGMM and ALTMM have a product (Kcat*E0) that cannot be decomposed from rate data alone.
    # E0 must be locked to a known value for the fit to be identifiable.
    if (model %in% c("OGMM", "ALTMM") && (is.null(locked.params) || !"E0" %in% locked.params)) {
        stop("For the ", PLOT_TITLES[[model]], " model, E0 must be locked to a known value (set locked.params = c('E0', ...)).")
    }
    # Models with an extra independent variable (e.g. I, B, R) need more than one unique level
    # of that variable for the corresponding parameter (e.g. Ki) to be identifiable.
    if (length(model.vars) > 2) {
        extra.var <- model.vars[3]
        n.unique <- length(unique(data.df[[extra.var]]))
        if (n.unique < 2) {
            stop("The ", PLOT_TITLES[[model]], " model requires at least 2 unique values of ", extra.var, " in the data.")
        }
    }
    # ===============================
    
    
    # Add minor noise =========================
    if (add.minor.noise) {
        # For reproducibility of the jitter. Restore the caller's RNG afterwards.
        had.seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        saved.seed <- if (had.seed) get(".Random.seed", envir = .GlobalEnv) else NULL
        set.seed(24)
        dependent.var.name <- MODEL_VARIABLES[[model]][2]
        noise <- rnorm(n = nrow(data.df), mean = 0, sd = 1e-6)
        data.df[[dependent.var.name]] <- data.df[[dependent.var.name]] + noise
        if (had.seed) {
            assign(".Random.seed", saved.seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
            rm(".Random.seed", envir = .GlobalEnv)
        }
    }
    # ===============================
    
    
    # Set up model ready to fit ================
    # Extract the variables according to the chosen model
    param.names <- MODEL_PARAMETERS[[model]]
    full.model.name <- PLOT_TITLES[[model]]
    model.params.string <- MODEL_PARAMETER_STRINGS[[model]]
    # Initialise empty statistics
    statistics <- NULL
    # ===============================
    
    
    
    # Initialize the results
    fitted.params <- NULL
    statistics <- NULL
    errorMessage <- NULL
    ci.ok <- FALSE
    # If nonlinear least squares ================
    if (fit.method == "nls") {
        
        # NLS specific error handling ======
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
        # Ensure not all parameters are locked (nothing left to fit)
        if (!is.null(locked.params) && all(param.names %in% locked.params)) {
            stop("All parameters are locked. Unlock at least one parameter to fit.")
        }
        # ===============================
        
        # Define model
        model.formula <- MODEL_FORMULAE[[model]]
        # Hold locked parameters constant by supplying them as data, not as nls parameters
        if (!is.null(locked.params)) {
            for (locked.param in locked.params) {
                data.df[[locked.param]] <- start.params[[locked.param]]
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
        tryCatch({
            # Fit with NLS
            fit <- nls(model.formula, data = data.df, start = fit.start.params, control = ctrl)
            fitted.params <- as.list(coef(fit))
            names(fitted.params) <- names(coef(fit))
            # Get confidence intervals
            if (get.conf.int) {
                # Compute the confidence intervals using the chosen method.
                #  - "profile":    profile likelihood via stats::confint(), which dispatches
                #                  to MASS:::confint.nls. Preferred for nonlinear fits as it
                #                  captures asymmetric parameter uncertainty, but it can fail
                #                  or return non-finite bounds for weakly identified parameters.
                #  - "asymptotic": Wald / standard-error intervals via nlstools::confint2().
                #                  Fast and symmetric, but can cross impossible parameter ranges.
                confidence.intervals <- tryCatch(
                    switch(
                        conf.method,
                        profile = {
                            # Profile likelihood relies on MASS's confint.nls S3 method.
                            if (!requireNamespace("MASS", quietly = TRUE)) {
                                stop("Package 'MASS' is required for profile likelihood confidence intervals.")
                            }
                            suppressMessages(stats::confint(fit, level = conf.level))
                        },
                        asymptotic = nlstools::confint2(fit, level = conf.level)
                    ),
                    error = function(e) {
                        message(paste0("Confidence interval computation failed: ", e$message, ". "))
                        NULL
                    }
                )
                # With a single free parameter, profile confint() returns a plain
                # (lower, upper) vector instead of a one-row matrix. Make it a matrix
                # so the bounds are unpacked by parameter name below.
                if (!is.null(confidence.intervals) && !is.matrix(confidence.intervals)) {
                    confidence.intervals <- matrix(confidence.intervals, nrow = 1,
                                                   dimnames = list(names(coef(fit)), names(confidence.intervals)))
                }
                # A failed or degenerate CI (an error, or any non-finite bound) is not a valid
                # statistical result. Keep the parameter estimates and report the CI failure.
                if (is.null(confidence.intervals) || !all(is.finite(confidence.intervals))) {
                    errorMessage <- if (conf.method == "profile") {
                        "Profile likelihood confidence intervals could not be computed. Try the Asymptotic CI method."
                    } else {
                        "Asymptotic confidence intervals could not be computed. The fit may be poorly conditioned."
                    }
                } else {
                    # Unpack into fitted params (e.g. Km.lb and Km.ub). Bounds are reported
                    # exactly as returned, including non-positive lower bounds; any clamping
                    # for plotting is the caller's responsibility (see EKA's confidence envelope).
                    for (param in rownames(confidence.intervals)) {
                        lower_bound <- confidence.intervals[param, 1]
                        upper_bound <- confidence.intervals[param, 2]
                        fitted.params[[paste0(param, ".lb")]] <- lower_bound
                        fitted.params[[paste0(param, ".ub")]] <- upper_bound
                    }
                    ci.ok <- TRUE
                }
            }
            if (get.stats && !is.null(fitted.params)) {
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
                statistics$Log.likelihood <- glance$logLik
                # Record confidence level and CI method only when bounds were successfully computed
                if (ci.ok) {
                    statistics$Confidence.level <- conf.level
                    statistics$CI.method <- conf.method
                }
            }
            
        }, error = function(e) {
            # Message for web page
            errorMessage <<- paste("Model fitting failed. Possible reasons: noiseless data, poor starting parameters, over-parameterisation, etc.")
            # Message for console
            message(paste0("Model fitting failed: ", e$message,". "))
        })
        # Return locked.params ===================
        # Add locked.params to fitted.params
        if (!is.null(fitted.params) && !is.null(locked.params)) {
            for (locked.param in locked.params) {
                locked.value <- start.params[[locked.param]]
                fitted.params[locked.param] <- locked.value
                # A locked parameter has a CI of [value, value]
                if (ci.ok) {
                    fitted.params[[paste0(locked.param, ".lb")]] <- locked.value
                    fitted.params[[paste0(locked.param, ".ub")]] <- locked.value
                }
            }
        }
    }
    # ===============================
    
    
    # If recursive method ================
    # TODO: this solver needs review. It is not actually recursive.
    # After the first estimate it replaces measured V with a perfect MM curve
    # so later iterations cannot change the answer. Treat as a one-shot closed form until rewritten.
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
    
    
    # If sum of squares calculation method =========
    if (fit.method == "ss.calc") {
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
    
    
    # If non-parametric method ===========
    if (fit.method == "nonparametric") {
        # Define a small epsilon value
        eps <- 1e-10
        # Initialise empty lists
        all.est.Vmax <- c()
        all.est.Km <- c()
        # Loop through every pair of observations
        for (i in 1:nrow(data.df)) {
            for (j in 1:nrow(data.df)) {
                # Skip tied A and origin / zero-rate points (those pairs inject mixed-sign Km, Vmax)
                if (data.df$A[i] != data.df$A[j] &&
                    data.df$A[i] > 0 && data.df$A[j] > 0 &&
                    data.df$V[i] > 0 && data.df$V[j] > 0) {
                    # Extract values Ai, Vi, Aj, Vj
                    Ai <- data.df$A[i]
                    Vi <- data.df$V[i]
                    Aj <- data.df$A[j]
                    Vj <- data.df$V[j]
                    # Compute Km and Vmax values for this pair of observations
                    est.Vmax <- (Ai - Aj) / ((Ai / (Vi + eps)) - (Aj / (Vj + eps)))
                    est.Km <- (Vj - Vi) / ((Vi / (Ai + eps)) - (Vj / (Aj + eps)))
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
        if (length(all.est.Km) == 0) {
            fitted.params <- NULL
            errorMessage <- "Nonparametric fitting needs at least two observations with different, positive A and V."
        } else {
            # Find medians of both (if even number of obs, take mean of centre two)
            median.est.Vmax <- median(all.est.Vmax)
            median.est.Km <- median(all.est.Km)
            # Return estimates
            fitted.params <- list("Km" = median.est.Km, "Vmax" = median.est.Vmax)
        }
    }
    # ===============================
    
    
    # If any fitted parameter estimates are out of valid range (ignore CI bounds)
    if (!is.null(fitted.params)) {
        param.values <- unlist(fitted.params[param.names])
        if (any(param.values <= 0) || any(param.values > 1e19)) {
            fitted.params <- NULL
            errorMessage <- "Model fitting returned parameter values out of valid range."
            message(errorMessage)
        }
    } 
    
    # Return ===============
    # Return the results, including any error message
    return(list(fitted.params = fitted.params, statistics = statistics, error = errorMessage))
}
