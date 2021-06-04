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
  print("Starting Given.Params")

  ## Setup
  measurements = params$measurements
  spacing = params$spacing
  KmA = params$KmA
  KmB = params$KmB
  Ksat = params$Ksat
  Vmax = params$Vmax
  A.min = params$Arange[1]
  A.max = params$Arange[2]
  if (spacing == "lin") {
    A.range = pracma::linspace(A.min, A.max, n = measurements)
  } else if (spacing == "log") {
    if (A.min == 0) {
      A.min = .Machine$double.eps
    }
    A.range = pracma::logspace(log10(A.min), log10(A.max), n = measurements)
  }
  B.values = params$Bvalues
  noise = params$noise
  noise.type = params$noise.type

  A.len = length(A.range)
  B.len = length(B.values)
  A.rep.B = rep(A.range, times = B.len)
  B.rep.A = rep(B.values, each = A.len)

  run.count = 1

  print("Setup complete")


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

  print("Data created")

  # Noise
  if (model == "MM") {
    noise.vec =  rnorm(measurements, mean = 0, sd = 1)
  }
  else {
    noise.vec =  rnorm(length(A.rep.B), mean = 0, sd = 1)
  }

  if (noise.type == "Abs" & noise != 0) {
    model.data$V0 = model.data$V0 + noise*noise.vec
  }
  else if (noise.type == "Rel" & noise != 0) {
    model.data$V0 = model.data$V0 + model.data$V0*noise*noise.vec
  }
  else if (noise.type == "None" | noise == 0) {
    model.data$V0 = model.data$V0 + 1e-10      # add insignificant noise so that nls can fit properly
  }

  print("Noise added")


  # Apparent Fit
  plot.options = list(options = 1)
  if (model == "MM") {
    params = Michaelis.Menten(model.data,plot.options)

    if (length(params) == 1) {
      return(c(F,params))
    }

    Km.app = params[[1]]
    Vmax.app = params[[2]]
    LWB.plot.A = params[[4]]
    res.plot = params[[5]]
    stats = params[[6]]
    fit.data = params[[7]]
  }
  else if (model == "LCI") {
    params = LCI(model.data,plot.options)

    if (params[[1]] == F) {
      return(c(F,params))
    }

    KmA.app = params[[1]]
    Ki.app = params[[2]]
    Vmax.app = params[[3]]
    LWB.plot.A = params[[5]]
    res.plot = params[[6]]
    stats = params[[7]]
    fit.data = params[[8]]
  }
  else if (model == "TC") {
    params = Ternary.complex(model.data,plot.options)

    if (length(params) == 1) {
      return(c(F,params))
    }

    KmA.app = params[[1]]
    KmB.app = params[[2]]
    Ksat.app = params[[3]]
    Vmax.app = params[[4]]
    enz.plot.B = params[[6]]
    LWB.plot.A = params[[7]]
    LWB.plot.B = params[[8]]
    res.plot = params[[9]]
    stats = params[[10]]
    fit.data = params[[11]]
  }
  else if (model == "PP") {
    params = Ping.pong(model.data,plot.options)

    if (length(params) == 1) {
      return(c(F,params))
    }

    KmA.app = params[[1]]
    KmB.app = params[[2]]
    Vmax.app = params[[3]]
    enz.plot.B = params[[5]]
    LWB.plot.A = params[[6]]
    LWB.plot.B = params[[7]]
    res.plot = params[[8]]
    stats = params[[9]]
    fit.data = params[[10]]
  }

  print("Apparent fit complete")


  # Confidence interval
  # confints = nlstools::confint2(model)
  # KmA.2.5 = confints[1]
  # KmB.2.5 = confints[2]
  # Ksat.2.5 = confints[3]
  # Vmax.2.5 = confints[4]
  # KmA.97.5 = confints[5]
  # KmB.97.5 = confints[6]
  # Ksat.97.5 = confints[7]
  # Vmax.97.5 = confints[8]
  #
  # EK.data$V0.lb = Vmax.2.5*EK.data$A*EK.data$B /
  #   (KmA.97.5*EK.data$A + KmB.97.5*EK.data$B + EK.data$A*EK.data$B + Ksat.97.5*KmB.97.5)
  # EK.data$V0.ub = Vmax.97.5*EK.data$A*EK.data$B /
  #   (KmA.2.5*EK.data$A + KmB.2.5*EK.data$B + EK.data$A*EK.data$B + Ksat.2.5*KmB.2.5)




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
                        label = sprintf("Km Apparent = %.3f\nVmax Apparent = %.3f",              # stating the KmA and Vmax values
                                        Km.app,Vmax.app)) +
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
                        label = sprintf("Km Apparent = %.3f\nVmax Apparent = %.3f",              # stating the KmA and Vmax values
                                        KmA.app,Vmax.app)) +
      ggthemes::theme_few()                                                       # use the minimalist theme
  }

  if (model == "MM" | model == "LCI") {
    plots = list(enz.plot.A, LWB.plot.A, res.plot, stats)
  }
  else if (model == "TC" | model == "PP") {
    plots = list(enz.plot.A, enz.plot.B, LWB.plot.A, LWB.plot.B, res.plot, stats)
  }

  print("Plots created, returning")
  return(c(T,plots))
}
