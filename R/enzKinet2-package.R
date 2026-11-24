#' enzKinet2: models for enzyme kinetics
#'
#' @description
#' enzKinet2 simulates, fits and plots 14 models of enzyme and binding
#' kinetics. It is the engine behind EKA2, the enzyme kinetics web app at
#' <https://eka.nz>.
#'
#' The package has six functions:
#' * [simulate_data()] makes noisy data from a model and parameter values.
#' * [make_curve()] makes a model's curve (dense, noise-free values), with an
#'   optional parameter uncertainty envelope.
#' * [fit_model()] fits a model to data and returns parameter estimates,
#'   confidence intervals and fit statistics.
#' * [make_plot()] plots data and curves, with or without a linearising
#'   transformation.
#' * [make_residual_plot()] plots the residuals of data from a model.
#' * [update_plot_curve()] replaces the curve in an existing plot.
#'
#' The models, their parameters and their variables are listed in
#' [model_registry].
#'
#' Guides, worked examples and the EKA2 user guide are at
#' <https://eka.nz/docs>.
#'
#' @keywords internal
#' @importFrom utils globalVariables
"_PACKAGE"

# Column names used inside ggplot2::aes() for the direct linear plot
utils::globalVariables(c("x1", "y1", "x2", "y2"))
