## Function file for enzKinet2 package

#' Given Parameters function
#'
#' @author Daniel Mak
#' 24/03/2021
#'
#' This function creates plots of enzyme kinetics from given parameters
#' @param params a named list of parameters
#' @param model the model to use
#' @return list(plot)
#' @export

Given.Params = function(params,model) {
  ## Setup
  measurements = params$measurements
  KmA = params$KmA
  KmB = params$KmB
  Ksat = params$Ksat
  Vmax = params$Vmax
  A.min = params$Arange[1]
  A.max = params$Arange[2]
  A.range = pracma::linspace(A.min, A.max, n = measurements)
  B.values = params$Bvalues
  noise = params$noise
  noise.type = params$noise.type

  A.len = length(A.range)
  B.len = length(B.values)
  A.rep.B = rep(A.range, times = B.len)
  B.rep.A = rep(B.values, each = A.len)

  run.count = 1


  ## Process ----
  if (model == "MM") {
    model.data = data.frame(A = A.range,
                            V0 = Vmax*A.range/(KmA + A.range))
  }

  else if (model == "LCI") {
    model.data = data.frame(A = A.rep.B,
                            B = B.rep.A,
                            V0 = rep(0, times = length(A.rep.B)))

    for (B in B.values) {
      start.pos = 1 + A.len*(run.count - 1)
      end.pos = A.len*run.count

      V0 = Vmax*A.range/(KmA*(1 + B/KmB) + A.range)

      model.data[start.pos:end.pos,"V0"] = V0

      run.count = run.count + 1
    }
  }

  else if (model == "PP") {
    model.data = data.frame(A = A.rep.B,
                            B = B.rep.A,
                            V0 = rep(0, times = length(A.rep.B)))

    for (B in B.values) {
      start.pos = 1 + A.len*(run.count - 1)
      end.pos = A.len*run.count

      V0 = Vmax*A.range*B/(KmA*A.range + KmB*B + A.range*B)

      model.data[start.pos:end.pos,"V0"] = V0

      run.count = run.count + 1
    }
  }

  else if (model == "TC") {
    model.data = data.frame(A = A.rep.B,
                            B = B.rep.A,
                            V0 = rep(0, times = length(A.rep.B)))

    for (B in B.values) {
      start.pos = 1 + A.len*(run.count - 1)
      end.pos = A.len*run.count

      V0 = Vmax*A.range*B/(KmA*A.range + KmB*B + A.range*B + Ksat*KmB)

      model.data[start.pos:end.pos,"V0"] = V0

      run.count = run.count + 1
    }
  }

  # Noise
  noise.vec =  rnorm(length(A.rep.B), mean = 0, sd = 1)
  if (noise.type == "Abs") {
    model.data$V0 = model.data$V0 + noise*noise.vec
  }
  else if (noise.type == "Rel") {
    model.data$V0 = model.data$V0 + model.data$V0*noise*noise.vec
  }


  # Apparent Fit
  plot.options = list(options = 1)
  if (model == "MM") {
    params = Michaelis.Menten(model.data,plot.options)

    fit.data = params[[7]]
  }
  else if (model == "LCI") {
    params = LCI(model.data,plot.options)

    fit.data = params[[8]]
  }
  else if (model == "TC") {
    params = Ternary.complex(model.data,plot.options)

    fit.data = params[[11]]
  }
  else if (model == "PP") {
    params = Ping.pong(model.data,plot.options)

    fit.data = params[[10]]
  }



  ## Results ----
  if (model == "MM") {
    enz.plot.A =                                                                  # create a ggplot
      ggplot2::ggplot(model.data,                                                    # using EK.data
                      ggplot2::aes(A, V0)) +                                       # plot A vs V0
      ggplot2::geom_point() +                                                     # and plot as points
      ggplot2::geom_line(fit.data,
                         mapping = ggplot2::aes(A,V0),
                         inherit.aes = F) +
      ggplot2::geom_hline(yintercept = Vmax,                                      # add a horizontal line for Vmax
                          linetype = "dashed",
                          colour = "green") +
      ggplot2::geom_vline(xintercept = KmA,                                        # add a horizontal line for KmA
                          linetype = "dashed",
                          colour = "red") +
      ggplot2::xlab(sprintf("Cs1")) +
      ggplot2::ylab("Velocity") +
      ggplot2::ggtitle("Enzyme Kinetics") +
      ggplot2::labs(colour = "Legend") +                                          # rename the legend
      ggplot2::annotate(geom = "text",                                            # add a text annotation
                        x = median(A.range),                                      # in the approximate middle
                        y = median(Vmax/2),
                        label = sprintf("Km1 = %.3f\nVmax = %.3f",              # stating the KmA and Vmax values
                                        KmA,Vmax)) +
      ggthemes::theme_few()
  }
  else {
    enz.plot.A =                                                                  # create a ggplot
      ggplot2::ggplot(model.data,
                      ggplot2::aes(A, V0, colour = as.factor(B))) +                           # plot A vs V0, colouring based on their B value
      ggplot2::geom_point() +                                                     # and plot as points
      ggplot2::geom_line(fit.data,
                         mapping = ggplot2::aes(A, V0, colour = as.factor(B)),
                         inherit.aes = F) +
      ggplot2::geom_hline(yintercept = Vmax,                                      # add a horizontal line for Vmax
                          linetype = "dashed",
                          colour = "green") +
      ggplot2::geom_vline(xintercept = KmA,                                       # add a horizontal line for KmA
                          linetype = "dashed",
                          colour = "red") +
      ggplot2::xlab(sprintf("Cs1")) +
      ggplot2::ylab("Velocity") +
      ggplot2::ggtitle("Enzyme Kinetics") +
      ggplot2::labs(colour = "Legend") +                                          # rename the legend
      ggplot2::annotate(geom = "text",                                            # add a text annotation
                        x = median(A.range),                                      # in the approximate middle
                        y = median(Vmax/2),
                        label = sprintf("Km1 = %.3f\nVmax = %.3f",              # stating the KmA and Vmax values
                                        KmA,Vmax)) +
      ggthemes::theme_few()                                                       # use the minimalist theme
  }

  return(enz.plot.A)
}
