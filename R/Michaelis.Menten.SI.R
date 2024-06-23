## Function file for enzKinet2 package

#' Michaelis-Menten Model with Substrate inhibition
#'
#' @author Haig Bishop
#' 23/06/2024
#'
#' This function analyses enzyme kinetics data and reports on the results
#' @param EK.data a dataframe containing the enzyme kinetics data- two columns,
#' the first for substrate concentration and the second for velocity.
#' @param plot.options a list of plot options to use. Must include numeric
#' variable "options", which decides on the use of the base options
#' (options = 1) or the custom options (options = 2)
#' @return list(Km, Vmax, Ksi, model, stats)
#' Prints- Km, Vmax and Ksi
#' Plots - A vs V0, 1/A vs 1/V0, and A vs residuals
#' @export

Michaelis.Menten.SI = function(EK.data,plot.options,conf.level) {
  print("Starting Michaelis.Menten.SI")

  ## Setup ----
  # Standardise column names
  data.size = ncol(EK.data)               # find the number of columns in the data
  A.col = 1                               # expect that A data is in the first column
  V0.col = 2                              # expect that V0 data is in the second column

  if (data.size > 2) {                    # if their are more than 2 columns
    if ("V0" %in% colnames(EK.data)) {    # if V0 exists as one of the column names
      V0.col = match("V0",names(EK.data)) # change the V0.col number to the correct column
      if (V0.col == 1) {                  # if V0 data is in the first column
        A.col = 2                         # assume that A data is in the second column
      }
    } else {
      V0.col = data.size                  # assume that V0 data is in the last column
    }
  } else if (data.size < 2) {
    return("Error, data requires 2 or more columns")
  }

  name.1 = names(EK.data)[A.col] # store names for later use
  name.2 = names(EK.data)[V0.col]

  names(EK.data)[A.col] = "A"    # overwrite names
  names(EK.data)[V0.col] = "V0"

  # Extract plot options
  options.counter = plot.options$options
  if (options.counter == 1) {
    title.1 = "Enzyme Kinetics \nDirect plot"
    title.2 = "Enzyme Kinetics \nLineweaver-Burk"
    x.units = ""
    y.units = ""
    plot.mods = ""
  } else if (options.counter == 2) {
    if (plot.options$title.1 != "") {
      title.1 = plot.options$title.1
    } else {
      title.1 = "Enzyme Kinetics \nDirect plot"
    }

    if (plot.options$title.2 != "") {
      title.2 = plot.options$title.2
    } else {
      title.2 = "Enzyme Kinetics \nLineweaver-Burk"
    }

    if (plot.options$x.units != "") {
      x.units = plot.options$x.units
    } else {
      x.units = ""
    }

    if (plot.options$y.units != "") {
      y.units = plot.options$y.units
    } else {
      y.units = ""
    }
    plot.mods = plot.options$plot.mods
  }

  # Define model
  formu = formula(V0 ~ Vmax*A/(Km + A + A**2/Ksi))


  # Estimate starting parameters for regression
  Km.est = median(EK.data$A, na.rm = T) # use the median of measurements for Km values
  Vmax.est = max(EK.data$V0, na.rm = T) # use maximum measured value for V0
  Ksi.est = max(EK.data$A, na.rm = T) # use maximum measured value for A, plus 1, cubed
  ests = list(Km = Km.est,              # create a named list of estimates
              Vmax = Vmax.est,
              Ksi = Ksi.est)

  print(Km.est)
  print(Vmax.est)
  print(Ksi.est)
  # Range for fitted model
  A.low = min(EK.data$A)                             # lowest value of A which the model will be valid for
  A.high = max(EK.data$A)                            # highest value of A which the model will be valid for
  A.range = pracma::linspace(A.low, A.high, n = 100) # model validity range
  A.concs = unique(EK.data$A)                        # list of unique A concs in the experimental data
  num.A = length(A.range)                            # length of the A range vector


  # NLS control parameters
  nlc = nls.control(maxiter = 1e5, tol = 1e-5) # set regression controls


  print("Setup complete")

  ## Process ----
  # Non-linear least square regression
  model = tryCatch(                                                          # prevent code from breaking in case where the data cannot be fit
    expr = nls(formu, data = EK.data, start = ests, control = nlc), # perform regression
    error = function(cond) {
      print(cond)
      return(cond)
    }
  )


  if (!is.list(model)) {
    return(model)
  }

  Km = unname(coef(model)["Km"])                                  # extract fitted KmA value
  Vmax = unname(coef(model)["Vmax"])                              # extract fitted Vmax value
  Ksi = unname(coef(model)["Ksi"])                                # extract fitted Ksi value


  print("Parameter fits found")


  # Create data from fitted parameters
  EK.data$V0.fit = Vmax*EK.data$A/(Km + EK.data$A + (EK.data$A * EK.data$A)/Ksi) # calculate fitted results at the same points as the experimental data

  A.fit.df = data.frame(A = A.range,               # dataframe for results of the fitted model using A as the range for each B concentration
                        V0 = Vmax*A.range / (Km + A.range + (A.range * A.range)/Ksi))


  print("Model simulated over range")


  # Confidence interval
  if (conf.level != 0) {
    confints = nlstools::confint2(model, level = conf.level)
    Km.lb = confints[1]
    Vmax.lb = confints[2]
    Ksi.lb = confints[3]
    Km.ub = confints[4]
    Vmax.ub = confints[5]
    Ksi.ub = confints[6]

    EK.data$V0.lb = Vmax.lb*EK.data$A/(Km.ub + EK.data$A + (EK.data$A * EK.data$A)/Ksi.ub)
    EK.data$V0.ub = Vmax.ub*EK.data$A/(Km.lb + EK.data$A + (EK.data$A * EK.data$A)/Ksi.lb)
  }

  # Lineweaver-Burk
  EK.data$A.inv = 1/EK.data$A   # invert A concentrations
  EK.data$V0.inv = 1/EK.data$V0 # invert V0 velocities

  # A as range
  A.LWB.df = data.frame(A.inv = 1/A.range,
                        V0.inv = Km/(Vmax*A.range) + 1/Vmax + A.range/(Vmax*Ksi))


  print("Lineweaver-Burk performed")


  # Residuals
  EK.data$Resids = EK.data$V0 - EK.data$V0.fit


  print("Residuals calculated")




  ## Results ----
  cat(sprintf("Km is %.3f, \nVmax is %.3f, \nKsi is %.3f\n", Km, Vmax, Ksi)) # print a statement about results, extend as necessary


  # Figure 1 - enzyme kinetics, substrate one
  enz.plot.A =                                                                  # create a ggplot
    ggplot2::ggplot(EK.data,                                                    # using EK.data
                    ggplot2::aes(A, V0)) +                                       # plot A vs V0
    ggplot2::geom_point() +                                                     # and plot as points
    ggplot2::geom_line(A.fit.df,                                                # then, using A.fit.df
                       mapping = ggplot2::aes(A, V0),                           # add a line of A vs V0
                       inherit.aes = F) +
    ggplot2::xlab(sprintf("%s, %s",name.1,x.units)) +
    ggplot2::ylab(sprintf("Velocity, %s",y.units)) +
    ggplot2::ggtitle(title.1) +
    ggplot2::labs(colour = "Legend") +                                          # rename the legend
    ggthemes::theme_few()                                                       # use the minimalist theme

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
Vmax = %.3f
Ksi = %.3f",
name.1, Km, Vmax, Ksi))                                                              # stating the KmA, Vmax and Ksi values
  }

  if (conf.level != 0) {
    enz.plot.A = enz.plot.A +
      ggplot2::geom_ribbon(EK.data,
                           mapping = ggplot2::aes(x = A, ymin = V0.lb, ymax = V0.ub),
                           alpha = 0.2,
                           inherit.aes = F)
    if ("res.on.plots" %in% plot.mods) {
      enz.plot.A = enz.plot.A +
        ggplot2::annotate(geom = "text",                                            # add a text annotation
                          x = 1.01*Km,                                      # in the approximate middle
                          y = 0.99*max(EK.data$V0),
                          hjust = 0,
                          vjust = 1,
                          label = sprintf(
"Km %s = %.3f, (%.3f - %.3f, %.1f%%)
Vmax = %.3f, (%.3f - %.3f, %.1f%%)
Ksi = %.3f, (%.3f - %.3f, %.1f%%)",              # stating the KmA, Vmax and Ksi values
name.1,
Km,Km.lb,Km.ub, conf.level*100,
Vmax, Vmax.lb, Vmax.ub, conf.level*100,
Ksi, Ksi.lb, Ksi.ub, conf.level*100))
    }
  }


  # Figure 2 - Lineweaver-Burk, substrate one
  LWB.plot.A =                                                                  # create a ggplot
    ggplot2::ggplot(EK.data,                                                    # using EK.data
                    ggplot2::aes(A.inv, V0.inv,)) +                             # plot 1/A vs 1/V0
    ggplot2::geom_point() +                                                     # and plot as points
    ggplot2::geom_line(A.LWB.df,                                                # then, using A.LWB.df
                       mapping = ggplot2::aes(A.inv, V0.inv),                   # add a line of 1/A vs 1/V0
                       inherit.aes = F) +
    ggplot2::xlab(sprintf("1/%s",name.1)) +
    ggplot2::ylab(sprintf("1/V0")) +
    ggplot2::ggtitle(title.2) +
    ggthemes::theme_few()


  # Figure 3 - Residuals of model
  res.plot =
    ggplot2::ggplot(EK.data,
                    ggplot2::aes(A, Resids)) +
    ggplot2::geom_point() +
    ggplot2::xlab(sprintf("%s, %s",name.1,x.units)) +
    ggplot2::ylab(sprintf("Velocity error, %s",y.units)) +
    ggplot2::ggtitle("Residual error of model") +
    ggthemes::theme_few()


  # Get stats
  standard.error = unname(summary(model)$coefficients[,2])
  R2 = modelr::rsquare(model,EK.data)
  RMSE = modelr::rmse(model, EK.data)
  MAE = modelr::mae(model, EK.data)
  Glance = broom::glance(model)
  stats = list(Model = "MMSI",
               KmA = Km,
               KmA.se = standard.error[1],
               KmB = NA,
               KmB.se = NA,
               KI = NA,
               KI.se = NA,
               Ksat = NA,
               Ksat.se = NA,
               h = NA,
               h.se = NA,
               Vmax = Vmax,
               Vmax.se = standard.error[2],
               Ksi = Ksi,
               Ksi.se = standard.error[3],
               R2 = R2,
               RMSE = RMSE,
               MAE = MAE,
               AIC = Glance$AIC,
               BIC = Glance$BIC,
               logLik = Glance$logLik)

  # Return parameters
  if (conf.level != 0) {
    return(list(Km,Vmax,Ksi,enz.plot.A,LWB.plot.A,res.plot,stats,A.fit.df,confints))
  } else {
    return(list(Km,Vmax,Ksi,enz.plot.A,LWB.plot.A,res.plot,stats,A.fit.df,0))
  }
}
