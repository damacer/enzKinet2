## Function file for enzKinet2 package

#' Linear Competitive Inhibition Model
#'
#' @author Daniel Mak
#' 10/03/2021
#'
#' This function analyses enzyme kinetics and reports on the results, using the
#' linear competitive inhibition (LCI) model.
#' @param EK.data a dataframe containing the enzyme kinetics data- three
#' columns, the first and second for substrate and inhibitor concentrations, and
#' the third for velocity.
#' @return list(Km, Ki, Vmax, model, stats)
#' Prints- Km, Ki, Vmax
#' Plots - A vs Vmax, 1/A vs 1/Vmax, residuals
#' @export

LCI = function(EK.data) {
  ## Setup ----
  # Standardise column names
  name.1 = names(EK.data)[1] # store names for later use
  name.2 = names(EK.data)[2]
  name.3 = names(EK.data)[3]

  names(EK.data)[1] = "A"    # overwrite names
  names(EK.data)[2] = "I"
  names(EK.data)[3] = "V0"


  # Define model
  formu = formula(V0 ~ (Vmax*A/(Km*(1 + I/Ki) + A)))


  # Estimate starting parameters for regression
  Km.est = median(EK.data$A, na.rm = T)  # use the median of measurements for Km values
  Ki.est = median(EK.data$I, na.rm = T)
  Vmax.est = max(EK.data$V0, na.rm = T)  # use maximum measured value for V0
  ests = list(Km = Km.est,               # create a named list of estimates
              Ki = Ki.est,
              Vmax = Vmax.est)


  # Range for fitted model
  A.low = min(EK.data$A)                             # lowest value of A which the model will be valid for
  A.high = max(EK.data$A)                            # highest value of A which the model will be valid for
  A.range = pracma::linspace(A.low, A.high, n = 100) # model validity range
  A.concs = unique(EK.data$A)                        # list of unique A concs in the experimental data
  num.A = length(A.range)                            # length of the A range vector
  I.concs = unique(EK.data$I)
  num.I = length(I.concs)


  # NLS control parameters
  nlc = nls.control(maxiter = 1e5, tol = 1e-5) # set regression controls


  print("Setup complete")




  ## Process ----
  # Non-linear least square regression
  model = nls(formu, data = EK.data, start = ests, control = nlc) # perform regression
  Km = unname(coef(model)["Km"])                                  # extract fitted KmA value
  Ki = unname(coef(model)["Ki"])                                  # extract fitted Ksat value
  Vmax = unname(coef(model)["Vmax"])                              # extract fitted Vmax value


  print("Parameter fits found")


  # Create data from fitted parameters
  EK.data$V0.fit =                      # calculate fitted results at the same points as the experimental data
    Vmax*EK.data$A /
    (Km*(1 + EK.data$I/Ki) + EK.data$A)

  # A as range for each I concentration
  A.seqA = rep(A.range, times = length(num.I))     # vector containing A.range repeated num.B.range times
  I.seqA = paste(rep(I.concs, each = num.A))       # vector containing each value in B.concs repeated num.A.range times
  A.fit.df = data.frame(A = A.seqA,                      # dataframe for results of the fitted model using A as the range for each B concentration
                        I = I.seqA,
                        V0 = NA)
  counter = 0
  for (I.conc in I.concs) {
    V0.range =                                   # calculate fitted V0
      Vmax*A.range /
      (Km*(1 + I.conc/Ki) + A.range)
      start.pos = 1 + num.A*counter        # starting index
      end.pos = num.A*(1 + counter)        # ending index
      A.fit.df[start.pos:end.pos, "V0"] = V0.range # place data in the V0 column
      counter = counter + 1                      # increment counter
  }


  print("Model simulated over range")


  # Lineweaver-Burk
  EK.data$A.inv = 1/EK.data$A   # invert A concentrations
  EK.data$V0.inv = 1/EK.data$V0 # invert V0 velocities

  # A as range
  A.LWB.df = data.frame(A = A.seqA,                  # dataframe for 1/A vs 1/V0
                        I = as.factor(I.seqA),
                        A.inv = 1/A.range,
                        V0.inv = NA)
  counter = 0
  for (I.conc in I.concs) {
    V0.inv.I =                                       # inverted LCI equation
      (Km*(1 + I.conc/Ki)) /
      (A.range*Vmax) + 1/Vmax
    start.pos = 1 + num.A*counter                    # starting index
    end.pos = num.A*(1 + counter)                    # ending index
    A.LWB.df[start.pos:end.pos, "V0.inv"] = V0.inv.I # place data in the V0.inv column
    counter = counter + 1                            # increment counter
  }


  print("Lineweaver-Burk performed")


  # Residuals
  EK.data$Resids = EK.data$V0 - EK.data$V0.fit


  print("Residuals calculated")




  ## Results ----
  cat(sprintf("Km is %.3f, \nKi is %.3f, \nVmax is %.3f\n", Km, Ki, Vmax)) # print a statement about results, extend as necessary


  # Figure 1 - enzyme kinetics, substrate one
  enz.plot.A =                                                                  # create a ggplot
    ggplot2::ggplot(EK.data,                                                    # using EK.data
                    ggplot2::aes(A, V0, colour = as.factor(I))) +                           # plot A vs V0, colouring based on their I value
    ggplot2::geom_point() +                                                     # and plot as points
    ggplot2::geom_line(A.fit.df,                                                # then, using A.fit.df
                       mapping = ggplot2::aes(A, V0, colour = I),               # add a line of A vs V0, colouring based on their I value
                       inherit.aes = F) +
    ggplot2::geom_hline(yintercept = Vmax,                                      # add a horizontal line for Vmax
                        linetype = "dashed",
                        colour = "green") +
    ggplot2::geom_vline(xintercept = Km,                                       # add a horizontal line for Km
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
                    ggplot2::aes(A.inv, V0.inv, colour = as.factor(I))) +       # plot 1/A vs 1/V0, colouring based on their I value
    ggplot2::geom_point() +                                                     # and plot as points
    ggplot2::geom_line(A.LWB.df,                                                # then, using A.LWB.df
                       mapping = ggplot2::aes(A.inv, V0.inv, colour = I),       # add a line of 1/A vs 1/V0, colouring based on their I values
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
  return(list(Km,Ki,Vmax,enz.plot.A,LWB.plot.A,res.plot,stats))
}
