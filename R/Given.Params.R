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

Given.Params = function(params,model,conf.level,plot.mods) {
  print("Starting Given.Params")

  ## Setup ----
  measurements = params$measurements
  spacing = params$spacing
  KmA = params$KmA
  KmB = params$KmB
  Ksat = params$Ksat
  Vmax = params$Vmax
  A.min = params$Arange[1]
  A.max = params$Arange[2]
  h.co = params$h.co
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

  else if (model == "Hill") {
    model.data = data.frame(A = A.range,
                            V0 = Vmax*A.range^h.co/(KmA^h.co + A.range^h.co))
    print(model.data)
  }

  print("Data created")

  # Noise
  if (model == "MM" || model == "Hill") {
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
    params = Michaelis.Menten(model.data,plot.options,conf.level)

    if (length(params) == 1) {
      return(c(F,params))
    }

    Km.app = params[[1]]
    Vmax.app = params[[2]]
    LWB.plot.A = params[[4]]
    res.plot = params[[5]]
    stats = params[[6]]
    fit.data = params[[7]]
    confints = params[[8]]

    if (conf.level != 0) {
      Km.lb = confints[1]
      Vmax.lb = confints[2]
      Km.ub = confints[3]
      Vmax.ub = confints[4]

      model.data$V0.lb = Vmax.lb*model.data$A/(Km.ub + model.data$A)
      model.data$V0.ub = Vmax.ub*model.data$A/(Km.lb + model.data$A)

    } else {
      Km.lb = F
      Vmax.lb = F
      Km.ub = F
      Vmax.ub = F
    }

  } else if (model == "LCI") {
    params = LCI(model.data,plot.options,conf.level)

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
    confints = params[[9]]

    names(fit.data)[2] = "B" # convert to be in-line with TC and PP models (reduces need to rewrite code)

    if (conf.level != 0) {
      Km.lb = confints[1]
      Ki.lb = confints[2]
      Vmax.lb = confints[3]
      Km.ub = confints[4]
      Ki.ub = confints[5]
      Vmax.ub = confints[6]

      model.data$V0.lb = Vmax.lb*model.data$A /
        (Km.ub*(1 + model.data$B/Ki.lb) + model.data$A)
      model.data$V0.ub = Vmax.ub*model.data$A /
        (Km.lb*(1 + model.data$B/Ki.ub) + model.data$A)

    } else {
      Km.lb = F
      Ki.lb = F
      Vmax.lb = F
      Km.ub = F
      Ki.ub = F
      Vmax.ub = F
    }

  } else if (model == "TC") {
    params = Ternary.complex(model.data,plot.options,conf.level)

    if (params[1] == F) {
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
    confints = params[[13]]

    if (conf.level != 0) {
      KmA.lb = confints[1]
      KmB.lb = confints[2]
      Ksat.lb = confints[3]
      Vmax.lb = confints[4]
      KmA.ub = confints[5]
      KmB.ub = confints[6]
      Ksat.ub = confints[7]
      Vmax.ub = confints[8]

      model.data$V0.lb = Vmax.lb*model.data$A*model.data$B /
        (KmA.ub*model.data$A + KmB.ub*model.data$B + model.data$A*model.data$B + Ksat.ub*KmB.ub)
      model.data$V0.ub = Vmax.ub*model.data$A*model.data$B /
        (KmA.lb*model.data$A + KmB.lb*model.data$B + model.data$A*model.data$B + Ksat.lb*KmB.lb)
    } else {
      KmA.lb = F
      KmB.lb = F
      Ksat.lb = F
      Vmax.lb = F
      KmA.ub = F
      KmB.ub = F
      Ksat.ub = F
      Vmax.ub = F
    }

  } else if (model == "PP") {
    params = Ping.pong(model.data,plot.options,conf.level)

    if (params[1] == F) {
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
    confints = params[[12]]

    if (conf.level != 0) {
      KmA.lb = confints[1]
      KmB.lb = confints[2]
      Vmax.lb = confints[3]
      KmA.ub = confints[4]
      KmB.ub = confints[5]
      Vmax.ub = confints[6]

      model.data$V0.lb = Vmax.lb*model.data$A*model.data$B /
        (KmA.ub*model.data$A + KmB.ub*model.data$B + model.data$A*model.data$B)
      model.data$V0.ub = Vmax.ub*model.data$A*model.data$B /
        (KmA.lb*model.data$A + KmB.lb*model.data$B + model.data$A*model.data$B)
    } else {
      KmA.lb = F
      KmB.lb = F
      Vmax.lb = F
      KmA.ub = F
      KmB.ub = F
      Vmax.ub = F
    }
  } else if (model == "Hill") {
    params = Hill(model.data,plot.options,conf.level)

    if (params[1] == F) {
      return(c(F,params))
    }

    Km.app = params[[1]]
    Vmax.app = params[[2]]
    h.co.app = params[[3]]
    enz.plot.A = params[[4]]
    LWB.plot.A = params[[5]]
    res.plot = params[[6]]
    stats = params[[7]]
    fit.data = params[[8]]
    confints = params[[9]]

    if (conf.level != 0) {
      Km.lb = confints[1]
      Vmax.lb = confints[2]
      h.co.lb = confints[3]
      Km.ub = confints[4]
      Vmax.ub = confints[5]
      h.co.ub = confints[6]

      model.data$V0.lb = Vmax.lb*model.data$A^h.co.lb/(Km.ub^h.co.ub + model.data$A^h.co.ub)
      model.data$V0.ub = Vmax.ub*model.data$A^h.co.ub/(Km.lb^h.co.lb + model.data$A^h.co.lb)

    } else {
      Km.lb = F
      Vmax.lb = F
      h.co.lb = F
      Km.ub = F
      Vmax.ub = F
      h.co.ub = F
    }
  }

  print("Apparent fit complete")




  ## Results ----
  if (model == "MM" || model == "Hill") {
    enz.plot.A =                                                                  # create a ggplot
      ggplot2::ggplot(model.data,                                                    # using EK.data
                      ggplot2::aes(A, V0)) +                                       # plot A vs V0
      ggplot2::geom_point() +                                                     # and plot as points
      ggplot2::geom_line(fit.data,
                         mapping = ggplot2::aes(A,V0),
                         inherit.aes = F) +
      ggplot2::xlab(sprintf("CsA")) +
      ggplot2::ylab("Velocity") +
      ggplot2::ggtitle("Enzyme Kinetics") +
      ggplot2::labs(colour = "Legend") +                                          # rename the legend
      ggthemes::theme_few()

    if (conf.level != 0) {
      enz.plot.A = enz.plot.A +
        ggplot2::geom_ribbon(model.data,
                             mapping = ggplot2::aes(x = A, ymin = V0.lb, ymax = V0.ub),
                             alpha = 0.2,
                             inherit.aes = F)
    }

    if ("Km.line" %in% plot.mods) {
      enz.plot.A = enz.plot.A +
        ggplot2::geom_vline(xintercept = Km,                                      # add a horizontal line for KmA
                            linetype = "dashed",
                            colour = "red")
    }

    if ("Vmax.line" %in% plot.mods) {
      enz.plot.A = enz.plot.A +
        ggplot2::geom_hline(yintercept = Vmax,                                      # add a horizontal line for Vmax
                            linetype = "dashed",
                            colour = "green")
    }

    if ("res.on.plots" %in% plot.mods & conf.level == 0) {
      enz.plot.A = enz.plot.A +
        ggplot2::annotate(geom = "text",                                            # add a text annotation
                          x = 1.01*Km,                                      # in the approximate middle
                          y = 0.99*max(EK.data$V0),
                          hjust = 0,
                          vjust = 1,
                          label = sprintf(
"Km %s = %.3f
Vmax = %.3f",
name.1, Km, Vmax))                                                              # stating the KmA and Vmax values
  }
  else {
    enz.plot.A =                                                                  # create a ggplot
      ggplot2::ggplot(model.data,
                      ggplot2::aes(A, V0, colour = as.factor(B))) +                           # plot A vs V0, colouring based on their B value
      ggplot2::geom_point() +                                                     # and plot as points
      ggplot2::geom_line(fit.data,
                         mapping = ggplot2::aes(A, V0, colour = as.factor(B)),
                         inherit.aes = F) +
      ggplot2::xlab(sprintf("CsA")) +
      ggplot2::ylab("Velocity") +
      ggplot2::ggtitle("Enzyme Kinetics") +
      ggthemes::theme_few()                                                       # use the minimalist theme

    if (model == "LCI") { # rename the legend
      enz.plot.A = enz.plot.A + ggplot2::labs(colour = "Inhibitor")
    } else {
      enz.plot.A = enz.plot.A + ggplot2::labs(colour = "B")
    }

    if (conf.level != 0) {
      enz.plot.A = enz.plot.A +
        ggplot2::geom_ribbon(model.data,
                             mapping = ggplot2::aes(x = A,
                                                    ymin = V0.lb, ymax = V0.ub,
                                                    colour = as.factor(B)),
                             alpha = 0.2,
                             inherit.aes = F)
    }

    if ("Km.line" %in% plot.mods) {
      enz.plot.A = enz.plot.A +
        ggplot2::geom_vline(xintercept = Km,                                      # add a horizontal line for KmA
                            linetype = "dashed",
                            colour = "red")
    }

    if ("Vmax.line" %in% plot.mods) {
      enz.plot.A = enz.plot.A +
        ggplot2::geom_hline(yintercept = Vmax,                                      # add a horizontal line for Vmax
                            linetype = "dashed",
                            colour = "green")
    }

    if ("res.on.plots" %in% plot.mods & conf.level == 0) {
      enz.plot.A = enz.plot.A +
        ggplot2::annotate(geom = "text",                                            # add a text annotation
                          x = 1.01*Km,                                      # in the approximate middle
                          y = 0.99*max(EK.data$V0),
                          hjust = 0,
                          vjust = 1,
                          label = sprintf(
"Km %s = %.3f
Vmax = %.3f",
name.1, Km, Vmax))                                                              # stating the KmA and Vmax values
    }
  }

  if (model == "MM" | model == "LCI" | model == "Hill") {
    plots = list(enz.plot.A, LWB.plot.A, res.plot, stats)
  }
  else if (model == "TC" | model == "PP") {
    plots = list(enz.plot.A, enz.plot.B, LWB.plot.A, LWB.plot.B, res.plot, stats)
  }

  print("Plots created, returning")
  return(c(T,plots))
}
