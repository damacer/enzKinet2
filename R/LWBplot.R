## Plotting function for enzKinet2 package

#' Lineweaver-Burk plot function
#'
#' @author Daniel Mak
#' 21/04/2021
#'
#' This function creates LWB plots for the enzKinet2 model functions
#' @param EK.data a data-frame containing the primary data to be plotted
#' @param fit.data a data-frame containing the fitted data to be plotted
#' @param labels a list of 2 plot labels (x-axis and y-axis)
#' @param title a string for the title of the plot
#' @param AB a string ("A" or "B") to choose which data-frame columns to use
#'
#' @export

LWBplot = function(EK.data, fit.data, labels, title, AB) {
  if (AB == "A") {
    col1.data = 7
    col2.data = 2
    col1.fit = 3
    col2.fit = 2
  } else if (AB == "B") {
    col1.data = 8
    col2.data = 1
    col1.fit = 3
    col2.fit = 1
  }

  x.lab = labels[[1]]
  y.lab = labels[[2]]

  plot =
    ggplot2::ggplot(EK.data,
                    ggplot2::aes(EK.data[,col1.data],
                                 V0.inv,
                                 colour = as.factor(EK.data[,col2.data]))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(fit.data,
                       mapping = ggplot2::aes(fit.data[,col1.fit],
                                              V0.inv,
                                              colour = as.factor(fit.data[,col2.fit])),
                       inherit.aes = F) +
    ggplot2::xlab(x.lab) +
    ggplot2::ylab(y.lab) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(colour = "Legend") +
    ggthemes::theme_few()

  print(plot)
  return(plot)
}
