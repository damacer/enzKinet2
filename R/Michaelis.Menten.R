## Function file for enzKinet2 package

#' Michaelis-Menten Model
#'
#' @author Daniel Mak
#' 10/03/2021
#'
#' This function analyses enzyme kinetics data and reports on the results
#' @param EK.data a dataframe containing the enzyme kinetics data- two columns,
#' the first for substrate concentration and the second for velocity.
#' @return list(Km, Vmax, model, stats)
#' Prints- Km and Vmax
#' Plots - A vs V0, 1/A vs 1/V0, and A vs residuals
#' @export

Michaelis.Menten = function(EK.data) {
  ## Setup ----
  # Standardise column names
  data.size = ncol(EK.data)               # find the number of columns in the data
  A.col = 1                               # expect that A data is in the first column
  V0.col = 2                              # expect that V0 data is in the second column

  if (data.size != 2) {                   # if their are more than 2 columns
    if ("V0" %in% colnames(EK.data)) {    # if V0 exists as one of the column names
      V0.col = match("V0",names(EK.data)) # change the V0.col number to the correct column
      if (V0.col == 1) {                  # if V0 data is in the first column
        A.col = 2                         # assume that A data is in the second column
      }
    } else {                              # if there is no column named V0
      V0.col = data.size                  # assume that V0 data is in the last column
    }
  }

  name.1 = names(EK.data)[A.col] # store names for later use
  name.2 = names(EK.data)[V0.col]

  names(EK.data)[A.col] = "A"    # overwrite names
  names(EK.data)[V0.col] = "V0"


  # Define model
  formu = formula(V0 ~ Vmax*A/(Km + A))


  # Estimate starting parameters for regression
  Km.est = median(EK.data$A, na.rm = T) # use the median of measurements for Km values
  Vmax.est = max(EK.data$V0, na.rm = T) # use maximum measured value for V0
  ests = list(Km = Km.est,              # create a named list of estimates
              Vmax = Vmax.est)


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
  model = nls(formu, data = EK.data, start = ests, control = nlc) # perform regression
  Km = unname(coef(model)["Km"])                                  # extract fitted KmA value
  Vmax = unname(coef(model)["Vmax"])                              # extract fitted Vmax value


  print("Parameter fits found")


  # Create data from fitted parameters
  EK.data$V0.fit = Vmax*EK.data$A/(Km + EK.data$A) # calculate fitted results at the same points as the experimental data

  A.fit.df = data.frame(A = A.range,               # dataframe for results of the fitted model using A as the range for each B concentration
                        V0 = Vmax*A.range / (Km + A.range))


  print("Model simulated over range")


  # Lineweaver-Burk
  EK.data$A.inv = 1/EK.data$A   # invert A concentrations
  EK.data$V0.inv = 1/EK.data$V0 # invert V0 velocities

  # A as range
  A.LWB.df = data.frame(A.inv = 1/A.range,
                        V0.inv = Km/(Vmax*A.range) + 1/Vmax)


  print("Lineweaver-Burk performed")


  # Residuals
  EK.data$Resids = EK.data$V0 - EK.data$V0.fit


  print("Residuals calculated")




  ## Results ----
  cat(sprintf("Km is %.3f, \nVmax is %.3f\n", Km, Vmax)) # print a statement about results, extend as necessary


  # Figure 1 - enzyme kinetics, substrate one
  enz.plot.A =                                                                  # create a ggplot
    ggplot2::ggplot(EK.data,                                                    # using EK.data
                    ggplot2::aes(A, V0)) +                                       # plot A vs V0
    ggplot2::geom_point() +                                                     # and plot as points
    ggplot2::geom_line(A.fit.df,                                                # then, using A.fit.df
                       mapping = ggplot2::aes(A, V0),                           # add a line of A vs V0
                       inherit.aes = F) +
    ggplot2::geom_hline(yintercept = Vmax,                                      # add a horizontal line for Vmax
                        linetype = "dashed",
                        colour = "green") +
    ggplot2::geom_vline(xintercept = Km,                                        # add a horizontal line for KmA
                        linetype = "dashed",
                        colour = "red") +
    ggplot2::xlab(sprintf("%s, mM",name.1)) +
    ggplot2::ylab("Velocity, µM/min/mg.enz") +
    ggplot2::ggtitle("Enzyme Kinetics \nModel fitted to data") +
    ggplot2::labs(colour = "Legend") +                                          # rename the legend
    ggplot2::annotate(geom = "text",                                            # add a text annotation
                      x = median(A.range),                                      # in the approximate middle
                      y = median(Vmax/2),
                      label = sprintf("Km %s = %.3f\nVmax = %.3f",              # stating the KmA and Vmax values
                                      name.1,Km,Vmax)) +
    ggthemes::theme_few()                                                       # use the minimalist theme


  # Figure 2 - Lineweaver-Burk, substrate one
  LWB.plot.A =                                                                  # create a ggplot
    ggplot2::ggplot(EK.data,                                                    # using EK.data
                    ggplot2::aes(A.inv, V0.inv,)) +                             # plot 1/A vs 1/V0
    ggplot2::geom_point() +                                                     # and plot as points
    ggplot2::geom_line(A.LWB.df,                                                # then, using A.LWB.df
                       mapping = ggplot2::aes(A.inv, V0.inv),                   # add a line of 1/A vs 1/V0
                       inherit.aes = F) +
    ggplot2::xlab(sprintf("1/%s",name.1)) +
    ggplot2::ylab("1/V0") +
    ggplot2::ggtitle("Lineweaver-Burk") +
    ggthemes::theme_few()


  # Figure 3 - Residuals of model
  res.plot =
    ggplot2::ggplot(EK.data,
                    ggplot2::aes(A, Resids)) +
    ggplot2::geom_point() +
    ggplot2::xlab(sprintf("%s, mM",name.1)) +
    ggplot2::ylab("Velocity error") +
    ggplot2::ggtitle("Residual error of model") +
    ggthemes::theme_few()


  # Get stats
  R2 = modelr::rsquare(model,EK.data)
  RMSE = modelr::rmse(model, EK.data)
  MAE = modelr::mae(model, EK.data)
  Glance = broom::glance(model)
  stats = list(Model = "MM",
               R2 = R2,
               RMSE = RMSE,
               MAE = MAE,
               AIC = Glance$AIC,
               BIC = Glance$BIC,
               logLik = Glance$logLik)


  # Return parameters
  return(list(Km,Vmax,enz.plot.A,LWB.plot.A,res.plot,stats))
}
