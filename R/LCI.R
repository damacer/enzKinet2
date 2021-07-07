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
#' @param plot.options a list of plot options to use. Must include numeric
#' variable "options", which decides on the use of the base options
#' (options = 1) or the custom options (options = 2)
#' @return list(Km, Ki, Vmax, model, stats)
#' Prints- Km, Ki, Vmax
#' Plots - A vs Vmax, 1/A vs 1/Vmax, residuals
#' @export

LCI = function(EK.data,plot.options,conf.level) {
  print("Starting LCI")

  ## Setup ----
  # Standardise column names
  data.size = ncol(EK.data)               # find the number of columns in the data
  A.col = 1                               # expect that A data is in the first column
  I.col = 2                               # expect that I data is in the second column
  V0.col = 3                              # expect that V0 data is in the third column

  if (data.size > 3) {                    # if their are more than 3 columns
    if ("V0" %in% colnames(EK.data)) {    # if V0 exists as one of the column names
      V0.col = match("V0",names(EK.data)) # change the V0.col number to the correct column
      if (V0.col == 1) {                  # if V0 data is in the first column
        A.col = 2                         # assume that A data is in the second column
        I.col = 3                         # assume that I data is in the third column
      }
    } else {
      V0.col = data.size                  # assume that V0 data is in the last column
    }
  } else if (data.size < 3) {
    return("Error, data requires 3 or more columns")
  }


  name.1 = names(EK.data)[A.col] # store names for later use
  name.2 = names(EK.data)[I.col]
  name.3 = names(EK.data)[V0.col]

  names(EK.data)[A.col] = "A"    # overwrite names
  names(EK.data)[I.col] = "I"
  names(EK.data)[V0.col] = "V0"

  # Extract plot options
  x.units = plot.options$x.units
  y.units = plot.options$y.units
  if (plot.options$options == 1) {        # use the base options
    title.1 = "Enzyme Kinetics \nDirect plot"
    title.2 = "Enzyme Kinetics \nLineweaver-Burk"
    x.lab1 = sprintf("%s", name.1)
    x.lab2 = sprintf("%s", name.2)
    x.lab1.inv = sprintf("1/%s", name.1)
    x.lab2.inv = sprintf("1/%s", name.2)
    y.lab = sprintf("Velocity")
    y.lab.inv = sprintf("1/V0")
  } else if (plot.options$options == 2) { # use custom options
    title.1 = plot.options$title.1
    title.2 = plot.options$title.2
    x.lab1 = sprintf("%s, %s", name.1, x.units)
    x.lab2 = sprintf("%s, %s", name.2, x.units)
    x.lab1.inv = sprintf("1/%s, 1/%s", name.1, x.units)
    x.lab2.inv = sprintf("1/%s, 1/%s", name.2, x.units)
    y.lab = sprintf("Velocity, %s", y.units)
    y.lab.inv = sprintf("1/V0, 1/%s", y.units)
  }
  # options.counter = plot.options$options
  # if (options.counter == 1) {
  #   title.1 = "Enzyme Kinetics \nDirect plot"
  #   title.2 = "Enzyme Kinetics \nLineweaver-Burk"
  #   x.units = ""
  #   y.units = ""
  # } else if (options.counter == 2) {
  #   title.1 = plot.options$title.1
  #   title.2 = plot.options$title.2
  #   x.units = plot.options$x.units
  #   y.units = plot.options$y.units
  # }


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
  model = tryCatch(                                                          # prevent code from breaking in case where the data cannot be fit
    expr = nls(formu, data = EK.data, start = ests, control = nlc), # perform regression
    error = function(cond) {
      print(cond)
      return(c(F,cond))
    }
  )

  if (!is.list(model[[1]])) {
    print("nls failed, returning error statement")
    return(model)
  }

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


  # Confidence interval
  if (conf.level != 0) {
    confints = nlstools::confint2(model, level = conf.level)
    Km.2.5 = confints[1]
    Ki.2.5 = confints[2]
    Vmax.2.5 = confints[3]
    Km.97.5 = confints[4]
    Ki.97.5 = confints[5]
    Vmax.97.5 = confints[6]

    EK.data$V0.lb = Vmax.2.5*EK.data$A /
      (Km.97.5*(1 + EK.data$I/Ki.2.5) + EK.data$A)
    EK.data$V0.ub = Vmax.97.5*EK.data$A /
      (Km.2.5*(1 + EK.data$I/Ki.97.5) + EK.data$A)
  } else {
    Km.2.5 = F
    Ki.2.5 = F
    Vmax.2.5 = F
    Km.97.5 = F
    Ki.2.5 = F
    Vmax.97.5 = F
  }

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
  fig1.params = list(Km = Km, Km.2.5 = Km.2.5, Km.97.5 = Km.97.5,
                     Vmax = Vmax, Vmax.2.5 = Vmax.2.5, Vmax.97.5 = Vmax.97.5,
                     name = name.1)
  fig1.labs = list(x.lab1, y.lab)
  enz.plot.A = enzKinet2::Directplot(EK.data, A.fit.df, fig1.params, fig1.labs, title.1, "A")

  # Figure 2 - Lineweaver-Burk, substrate one
  fig2.labs = list(x.lab1.inv, y.lab.inv)
  LWB.plot.A = enzKinet2::LWBplot(EK.data, A.LWB.df, fig2.labs, title.2, "A")

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
  R2 = modelr::rsquare(model,EK.data)
  RMSE = modelr::rmse(model, EK.data)
  MAE = modelr::mae(model, EK.data)
  Glance = broom::glance(model)
  stats = list(Model = "LCI",
               R2 = R2,
               RMSE = RMSE,
               MAE = MAE,
               AIC = Glance$AIC,
               BIC = Glance$BIC,
               logLik = Glance$logLik)


  # Return parameters
  if (conf.level != 0) {
    return(list(Km,Ki,Vmax,enz.plot.A,LWB.plot.A,res.plot,stats,A.fit.df,confints))
  } else {
    return(list(Km,Ki,Vmax,enz.plot.A,LWB.plot.A,res.plot,stats,A.fit.df,0))
  }
}
