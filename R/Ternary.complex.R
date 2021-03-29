## Function file for enzKinet2 package

#' Ternary-Complex Model
#'
#' @author Daniel Mak
#' 10/03/2021
#'
#' This function analyses enzyme kinetics and reports on the results, using the
#' ternary-complex mechanism model.
#' @param EK.data a dataframe containing the enzyme kinetics data- three
#' columns, the first and second for substrate concentrations and the third for
#' velocity.
#' @return list(KmA, KmB, Ksat, Vmax, model, stats)
#' Prints- KmA, KmB, Ksat, Vmax
#' Plots - A vs Vmax, 1/A vs 1/Vmax, B vs Vmax, 1/B vs 1/Vmax, residuals
#' @export

Ternary.complex = function(EK.data,plot.options) {
  ## Setup ----
  # Standardise column names
  data.size = ncol(EK.data)               # find the number of columns in the data
  A.col = 1                               # expect that A data is in the first column
  B.col = 2                               # expect that B data is in the second column
  V0.col = 3                              # expect that V0 data is in the third column

  if (data.size > 3) {                    # if their are more than 3 columns
    if ("V0" %in% colnames(EK.data)) {    # if V0 exists as one of the column names
      V0.col = match("V0",names(EK.data)) # change the V0.col number to the correct column
      if (V0.col == 1) {                  # if V0 data is in the first column
        A.col = 2                         # assume that A data is in the second column
        B.col = 3                         # assume that B data is in the third column
      }
    } else {
      V0.col = data.size                  # assume that V0 data is in the last column
    }
  } else if (data.size < 3) {
    return("Error, data requires 3 or more columns")
  }

  name.1 = names(EK.data)[A.col] # store names for later use
  name.2 = names(EK.data)[B.col]
  name.3 = names(EK.data)[V0.col]

  names(EK.data)[A.col] = "A"    # overwrite names
  names(EK.data)[B.col] = "B"
  names(EK.data)[V0.col] = "V0"

  # Extract plot options
  options.counter = plot.options$options
  if (options.counter == 1) {
    title.1 = "Enzyme Kinetics \nDirect plot"
    title.2 = "Enzyme Kinetics \nLineweaver-Burk"
    x.units = ""
    y.units = ""
  } else if (options.counter == 2) {
    title.1 = plot.options$title.1
    title.2 = plot.options$title.2
    x.units = plot.options$x.units
    y.units = plot.options$y.units
  }

  # Define model
  formu = formula(V0 ~ (Vmax*A*B /
                          (KmA*A + KmB*B + A*B + Ksat*KmB)))


  # Estimate starting parameters for regression
  KmA.est = median(EK.data$A, na.rm = T) # use the median of measurements for Km values
  KmB.est = median(EK.data$B, na.rm = T) # use the median of measurements for Km values
  Ksat.est = 0.5                         # use 0.5 for Ksat values
  Vmax.est = max(EK.data$V0, na.rm = T)  # use maximum measured value for V0
  ests = list(KmA = KmA.est,             # create a named list of estimates
              KmB = KmB.est,
              Ksat = Ksat.est,
              Vmax = Vmax.est)


  # Range for fitted model
  A.low = min(EK.data$A)                             # lowest value of A which the model will be valid for
  A.high = max(EK.data$A)                            # highest value of A which the model will be valid for
  A.range = pracma::linspace(A.low, A.high, n = 100) # model validity range
  A.concs = unique(EK.data$A)                        # list of unique A concs in the experimental data
  num.A = length(A.range)                            # length of the A range vector

  B.low = min(EK.data$B)
  B.high = max(EK.data$B)
  B.range = pracma::linspace(B.low, B.high, n = 100)
  B.concs = unique(EK.data$B)
  num.B = length(B.range)



  # NLS control parameters
  nlc = nls.control(maxiter = 1e5, tol = 1e-5) # set regression controls


  print("Setup complete")




  ## Process ----
  # Non-linear least square regression
  model = tryCatch(                                                          # prevent code from breaking in case where the data cannot be fit
    expr = nls(formu, data = EK.data, start = ests, control = nlc), # perform regression
    error = function(cond) {
      print(cond)
      return(F)
    }
  )

  if (!is.list(model)) {
    return("Data could not be fit")
  }

  if (!is.list(model)) {
    return("Data could not be fit")
  }

  KmA = unname(coef(model)["KmA"])                                # extract fitted Km values
  KmB = unname(coef(model)["KmB"])
  Ksat = unname(coef(model)["Ksat"])                              # extract fitted Ksat value
  Vmax = unname(coef(model)["Vmax"])                              # extract fitted Vmax value


  print("Parameter fits found")


  # Create data from fitted parameters
  EK.data$V0.fit = Vmax*EK.data$A*EK.data$B /                     # calculate fitted results at the same points as the experimental data
                   (KmA*EK.data$A + KmB*EK.data$B + EK.data$A*EK.data$B + Ksat*KmB)

  # A as range for each B concentration
  A.seqA = rep(A.range, times = length(num.B))     # vector containing A.range repeated num.B times
  B.seqA = paste(rep(B.concs, each = num.A))       # vector containing each value in B.concs repeated num.A times
  A.fit.df = data.frame(A = A.seqA,                # dataframe for results of the fitted model using A as the range for each B concentration
                        B = B.seqA,
                        V0 = NA)
  counter = 0
  for (B.conc in B.concs) {
    V0.range = Vmax*A.range*B.conc /
               (KmA*A.range + KmB*B.conc + A.range*B.conc + Ksat*KmB)           # calculate fitted V0
    start.pos = 1 + num.A*counter                                               # starting index
    end.pos = num.A*(1 + counter)                                               # ending index
    A.fit.df[start.pos:end.pos, "V0"] = V0.range                                  # place data in the V0 column
    counter = counter + 1                                                       # increment counter
  }

  # B as range for each A concentration
  A.seqB = paste(rep(A.concs, each = num.B))       # vector containing A.range repeated num.B times
  B.seqB = rep(B.range, times = length(num.A))     # vector containing each value in B.concs repeated num.A times
  B.fit.df = data.frame(A = A.seqB,                # dataframe for results of the fitted model using B as the range for each A concentration
                        B = B.seqB,
                        V0 = NA)
  counter = 0
  for (A.conc in A.concs) {
    V0.range = Vmax*A.conc*B.range /
               (KmA*A.conc + KmB*B.range + A.conc*B.range + Ksat*KmB)           # calculate fitted V0
    start.pos = 1 + num.A*counter                                               # starting index
    end.pos = num.A*(1 + counter)                                               # ending index
    B.fit.df[start.pos:end.pos, "V0"] = V0.range                                  # place data in the V0 column
    counter = counter + 1                                                       # increment counter
  }


  print("Model simulated over range")


  # Lineweaver-Burk
  EK.data$A.inv = 1/EK.data$A   # invert A concentrations
  EK.data$B.inv = 1/EK.data$B
  EK.data$V0.inv = 1/EK.data$V0 # invert V0 velocities

  # A as range
  A.LWB.df = data.frame(A = A.seqA,            # dataframe for 1/A vs 1/V0
                        B = as.factor(B.seqA),
                        A.inv = 1/A.range,
                        V0.inv = NA)
  counter = 0
  for (B.conc in B.concs) {
    V0.inv.B = (KmA*A.range + KmB*B.conc + A.range*B.conc + Ksat*KmB) /
               (Vmax*A.range*B.conc)
      start.pos = 1 + num.A*counter                                             # starting index
      end.pos = num.A*(1 + counter)                                             # ending index
      A.LWB.df[start.pos:end.pos, "V0.inv"] = V0.inv.B                            # place data in the V0.inv column
      counter = counter + 1                                                     # increment counter
  }

  # B as range
  B.LWB.df = data.frame(A = as.factor(A.seqB),            # dataframe for 1/A vs 1/V0
                        B = B.seqB,
                        B.inv = 1/B.range,
                        V0.inv = NA)
  counter = 0
  for (A.conc in A.concs) {
    V0.inv.B = (KmA*A.conc + KmB*B.range + A.conc*B.range + Ksat*KmB) /
               (Vmax*A.conc*B.range)
    start.pos = 1 + num.B*counter                                               # starting index
    end.pos = num.B*(1 + counter)                                               # ending index
    B.LWB.df[start.pos:end.pos, "V0.inv"] = V0.inv.B                              # place data in the V0.inv column
    counter = counter + 1                                                       # increment counter
  }


  print("Lineweaver-Burk performed")


  # Residuals
  EK.data$Resids = EK.data$V0 - EK.data$V0.fit


  print("Residuals calculated")




  ## Results ----
  cat(sprintf("Km%s is %.3f, \nKm%s is %.3f, \nKsat is %.3f, \nVmax is %.3f\n", name.1, KmA, name.2, KmB, Ksat, Vmax)) # print a statement about results, extend as necessary


  # Figure 1 - enzyme kinetics, substrate one
  enz.plot.A =                                                                  # create a ggplot
    ggplot2::ggplot(EK.data,                                                    # using EK.data
                    ggplot2::aes(A, V0, colour = as.factor(B))) +                           # plot A vs V0, colouring based on their B value
    ggplot2::geom_point() +                                                     # and plot as points
    ggplot2::geom_line(A.fit.df,                                                # then, using A.fit.df
                       mapping = ggplot2::aes(A, V0, colour = B),               # add a line of A vs V0, colouring based on their B value
                       inherit.aes = F) +
    ggplot2::geom_hline(yintercept = Vmax,                                      # add a horizontal line for Vmax
                        linetype = "dashed",
                        colour = "green") +
    ggplot2::geom_vline(xintercept = KmA,                                       # add a horizontal line for KmA
                        linetype = "dashed",
                        colour = "red") +
    ggplot2::xlab(sprintf("%s, %s",name.1,x.units)) +
    ggplot2::ylab(sprintf("Velocity, %s",y.units)) +
    ggplot2::ggtitle("Enzyme Kinetics \nModel fitted to data") +
    ggplot2::labs(colour = "Legend") +                                          # rename the legend
    ggplot2::annotate(geom = "text",                                            # add a text annotation
                      x = median(A.range),                                      # in the approximate middle
                      y = median(Vmax/2),
                      label = sprintf("Km %s = %.3f\nVmax = %.3f",              # stating the KmA and Vmax values
                                      name.1,KmA,Vmax)) +
    ggthemes::theme_few()                                                       # use the minimalist theme


  # Figure 2 - Lineweaver-Burk, substrate one
  LWB.plot.A =                                                                  # create a ggplot
    ggplot2::ggplot(EK.data,                                                    # using EK.data
                    ggplot2::aes(A.inv, V0.inv, colour = as.factor(B))) +       # plot 1/A vs 1/V0, colouring based on their B value
    ggplot2::geom_point() +                                                     # and plot as points
    ggplot2::geom_line(A.LWB.df,                                                # then, using A.LWB.df
                       mapping = ggplot2::aes(A.inv, V0.inv, colour = B),       # add a line of 1/A vs 1/V0, colouring based on their B values
                       inherit.aes = F) +
    ggplot2::xlab(sprintf("1/%s, 1/%s",name.1,x.units)) +
    ggplot2::ylab(sprintf("1/V0, 1/%s",y.units)) +
    ggplot2::ggtitle("Lineweaver-Burk") +
    ggthemes::theme_few()


  # Figure 3 - enzyme kinetics, substrate two
  enz.plot.B =
    ggplot2::ggplot(EK.data,
                    ggplot2::aes(B, V0, colour = as.factor(A))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(B.fit.df,
                       mapping = ggplot2::aes(B, V0, colour = A),
                       inherit.aes = F) +
    ggplot2::geom_hline(yintercept = Vmax,
                        linetype = "dashed",
                        colour = "green") +
    ggplot2::geom_vline(xintercept = KmB,
                        linetype = "dashed",
                        colour = "red") +
    ggplot2::xlab(sprintf("%s, %s",name.1,x.units)) +
    ggplot2::ylab(sprintf("Velocity, %s",y.units)) +
    ggplot2::ggtitle("Enzyme Kinetics \nModel fitted to data") +
    ggplot2::labs(colour = "Legend") +
    ggplot2::annotate(geom = "text",
                      x = median(B.range),
                      y = median(Vmax/2),
                      label = sprintf("Km %s = %.3f\nVmax = %.3f",
                                      name.2,KmB,Vmax)) +
    ggthemes::theme_few()


  # Figure 4 - Lineweaver-Burk, substrate two
  LWB.plot.B =
    ggplot2::ggplot(EK.data,
                    ggplot2::aes(B.inv, V0.inv, colour = as.factor(A))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(B.LWB.df,
                       mapping = ggplot2::aes(B.inv, V0.inv, colour = A),
                       inherit.aes = F) +
    ggplot2::xlab(sprintf("1/%s, 1/%s",name.1,x.units)) +
    ggplot2::ylab(sprintf("1/V0, 1/%s",y.units)) +
    ggplot2::ggtitle("Lineweaver-Burk") +
    ggthemes::theme_few()


  # Figure 5 - Residuals of model
  res.plot =
    ggplot2::ggplot(EK.data,
                    ggplot2::aes(A, Resids)) +
    ggplot2::geom_point() +
    ggplot2::xlab(sprintf("%s, %s",name.1,x.units)) +
                    ggplot2::ylab(sprintf("Velocity error, %s",y.units)) +
                    ggplot2::ggtitle("Residual error of model") +
                    ggthemes::theme_few()


  # Get stats
  R2 = modelr::rsquare(model,EK.data)
  RMSE = modelr::rmse(model, EK.data)
  MAE = modelr::mae(model, EK.data)
  Glance = broom::glance(model)
  stats = list(Model = "TC",
               R2 = R2,
               RMSE = RMSE,
               MAE = MAE,
               AIC = Glance$AIC,
               BIC = Glance$BIC,
               logLik = Glance$logLik)


  # Return parameters
  return(list(KmA,KmB,Ksat,Vmax,enz.plot.A,enz.plot.B,LWB.plot.A,LWB.plot.B,res.plot,stats,A.fit.df,B.fit.df))
}
