## Plotting function for enzKinet2 package

#' Direct plot function
#'
#' @author Daniel Mak
#' 21/04/2021
#'
#' This function creates direct plots for the enzKinet2 model functions
#' @param EK.data a data-frame containing the primary data to be plotted
#' @param fit.data a data-frame containing the fitted data to be plotted
#' @param params parameters to be used in plotting
#' @param labels a list of 2 plot labels (x-axis and y-axis)
#' @param title a string for the title of the plot
#' @param AB a string ("A" or "B") to choose which data-frame columns to use
#'
#' @export

Directplot = function(EK.data, fit.data, params, labels, title, AB, legend.name, plot.mods) {
  if (AB == "A") {
    col1 = 1
    col2 = 2
  } else if (AB == "B") {
    col1 = 2
    col2 = 1
  }

  Km = params$Km
  Km.lb = params$Km.lb
  Km.ub = params$Km.ub
  name = params$name
  Vmax = params$Vmax
  Vmax.lb = params$Vmax.lb
  Vmax.ub = params$Vmax.ub

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
    ggplot2::xlab(x.lab) +
    ggplot2::ylab(y.lab) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(colour = legend.name) +
    ggthemes::theme_few()

  if (Km.lb == F) {
    if ("res.on.plots" %in% plot.mods) {
      plot = plot +
        ggplot2::annotate(geom = "text",
                          x = 1.01*Km,
                          y = 0.99*max(EK.data[,3]),
                          hjust = 0,
                          vjust = 1,
                          label = sprintf(
"Km %s = %.3f
Vmax = %.3f",
name, Km,
Vmax))
    }
  }
  else {
    plot = plot +
      ggplot2::geom_ribbon(EK.data,
                           mapping = ggplot2::aes(x = EK.data[,col1],
                                                  ymin = V0.lb, ymax = V0.ub,
                                                  colour = as.factor(EK.data[,col2])),
                           alpha = 0.2,
                           inherit.aes = F)
    if ("res.on.plots" %in% plot.mods) {
      plot = plot +
        ggplot2::annotate(geom = "text",
                          x = median(EK.data[,col1]),
                          y = max(EK.data[,3]),
                          label = sprintf(
"Km %s = %.3f, 2.5%% = %.3f, 97.5%% = %.3f
Vmax = %.3f, 2.5%% = %.3f, 97.5%% = %.3f",
name, Km, Km.lb, Km.ub,
Vmax, Vmax.lb, Vmax.ub))
    }
  }

  if ("Km.line" %in% plot.mods) {
    plot = plot +
      ggplot2::geom_vline(xintercept = Km,
                          linetype = "dashed",
                          colour = "red")
  }

  if ("Vmax.line" %in% plot.mods) {
    plot = plot +
      ggplot2::geom_hline(yintercept = Vmax,
                          linetype = "dashed",
                          colour = "green")
  }

  print(plot)
  return(plot)
}
