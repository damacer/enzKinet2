## Plotting function for enzKinet2 package

#' Direct plot function
#'
#' @author Daniel Mak
#' 21/04/2021
#'
#' This function creates plots for the enzKinet2 model functions
#' @param EK.data a data-frame containing the primary data to be plotted
#' @param fit.data a data-frame containing the fitted data to be plotted
#' @param params
#' @param labels a list of 2 plot labels (x-axis and y-axis)
#' @param AB a string ("A" or "B") to choose which data-frame columns to use
#'
#' @export

Directplot = function(EK.data, fit.data, params, labels, AB) {
  if (AB == "A") {
    col1 = 1
    col2 = 2
  } else if (AB == "B") {
    col1 = 2
    col2 = 1
  }

  Km = params$Km
  name = params$name
  Vmax = params$Vmax

  x.lab = labels[[1]]
  y.lab = labels[[2]]


  plot =
    ggplot2::ggplot(EK.data,
                    ggplot2::aes(EK.data[,col1],
                                 V0,
                                 colour = as.factor(EK.data[,col2]))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(fit.data,
                       mapping = ggplot2::aes(fit.data[,col1],
                                              V0,
                                              colour = as.factor(fit.data[,col2])),
                       inherit.aes = F) +
    ggplot2:geom_hline(yintercept = Vmax,
                       linetype = "dashed",
                       colour = "green") +
    ggplot2::geom_vline(xintercept = Km,
                        linetype = "dashed",
                        colour = "red") +
    ggplot2::xlab(x.lab) +
    ggplot2::ylab(y.lab) +
    ggplot2::ggtitle("Enzyme Kinetics \nModel fitted to data") +
    ggplot2::labs(colour = "Legend") +
    ggplot2::annotate(geom = "text",
                      x = 1,
                      y = 1,
                      label = sprintf("Km %s = %.3f\nVmax = %.3f",
                                      name.1, Km, Vmax)) +
    ggthemes::theme_few()
}
