## Function file for enzKinet2 package

#' Ping-Pong Model
#'
#' @author Daniel Mak
#' 11/03/2021
#'
#' This function analyses enzyme kinetics and reports on the results, using the
#' ping-pong mechanism model.
#' @param EK.data a dataframe containing the enzyme kinetics data- three
#' columns, the first and second for substrate concentrations and the third for
#' velocity.
#' @param plot.options a list of plot options to use. Must include numeric
#' variable "options", which decides on the use of the base options
#' (options = 1) or the custom options (options = 2)
#' @return list(KmA, KmB, Vmax, model, stats)
#' Prints- KmA, KmB, Vmax
#' Plots - A vs Vmax, 1/A vs 1/Vmax, B vs Vmax, 1/B vs 1/Vmax, residuals
#' @export

Ping.pong = function(EK.data,plot.options,conf.level) {
  print("Starting Ping.pong")

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
    plot.mods = ""
  } else if (plot.options$options == 2) { # use custom options
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

    if (x.units != "") {
      x.lab1 = sprintf("%s, %s", name.1, x.units)
      x.lab1.inv = sprintf("1/%s, 1/%s", name.1, x.units)
    } else {
      x.lab1 = sprintf("%s", name.1)
      x.lab1.inv = sprintf("1/%s", name.1)
    }

    if (x.units != "") {
      x.lab2 = sprintf("%s, %s", name.2, x.units)
      x.lab2.inv = sprintf("1/%s, 1/%s", name.2, x.units)
    } else {
      x.lab2 = sprintf("%s", name.2)
      x.lab2.inv = sprintf("1/%s", name.2)
    }

    if (y.units != "") {
      y.lab = sprintf("Velocity, %s", y.units)
      y.lab.inv = sprintf("1/V0, 1/%s", y.units)
    } else {
      y.lab = sprintf("Velocity")
      y.lab.inv = sprintf("1/V0")
    }
    plot.mods = plot.options$plot.mods
  }

  # Define model
  formu = formula(V0 ~ (Vmax*A*B /
                          (KmA*B + KmB*A + A*B)))


  # Estimate starting parameters for regression
  KmA.est = median(EK.data$A, na.rm = T) # use the median of measurements for Km values
  KmB.est = median(EK.data$B, na.rm = T) # use the median of measurements for Km values
  Vmax.est = max(EK.data$V0, na.rm = T)  # use maximum measured value for V0
  ests = list(KmA = KmA.est,             # create a named list of estimates
              KmB = KmB.est,
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
      return(c(F,cond))
    }
  )

  if (!is.list(model[[1]])) {
    print("nls failed, returning error statement")
    return(model)
  }

  KmA = unname(coef(model)["KmA"])                                # extract fitted Km values
  KmB = unname(coef(model)["KmB"])
  Vmax = unname(coef(model)["Vmax"])                              # extract fitted Vmax value


  print("Parameter fits found")


  # Create data from fitted parameters
  EK.data$V0.fit = Vmax*EK.data$A*EK.data$B /                     # calculate fitted results at the same points as the experimental data
    (KmA*EK.data$B + KmB*EK.data$A + EK.data$A*EK.data$B)

  # A as range for each B concentration
  A.seqA = rep(A.range, times = length(num.B))     # vector containing A.range repeated num.B times
  B.seqA = paste(rep(B.concs, each = num.A))       # vector containing each value in B.concs repeated num.A times
  A.fit.df = data.frame(A = A.seqA,                # dataframe for results of the fitted model using A as the range for each B concentration
                        B = B.seqA,
                        V0 = NA)
  counter = 0
  for (B.conc in B.concs) {
    V0.range = Vmax*A.range*B.conc /
      (KmB*A.range + KmA*B.conc + A.range*B.conc)                               # calculate fitted V0
    start.pos = 1 + num.A*counter                                               # starting index
    end.pos = num.A*(1 + counter)                                               # ending index
    A.fit.df[start.pos:end.pos, "V0"] = V0.range                                # place data in the V0 column
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
      (KmB*A.conc + KmA*B.range + A.conc*B.range)                               # calculate fitted V0
    start.pos = 1 + num.A*counter                                               # starting index
    end.pos = num.A*(1 + counter)                                               # ending index
    B.fit.df[start.pos:end.pos, "V0"] = V0.range                                # place data in the V0 column
    counter = counter + 1                                                       # increment counter
  }


  print("Model simulated over range")


  # Confidence interval
  if (conf.level != 0) {
    confints = nlstools::confint2(model, level = conf.level)
    KmA.lb = confints[1]
    KmB.lb = confints[2]
    Vmax.lb = confints[3]
    KmA.ub = confints[4]
    KmB.ub = confints[5]
    Vmax.ub = confints[6]

    EK.data$V0.lb = Vmax.lb*EK.data$A*EK.data$B /
      (KmB.ub*EK.data$A + KmA.ub*EK.data$B + EK.data$A*EK.data$B)
    EK.data$V0.ub = Vmax.ub*EK.data$A*EK.data$B /
      (KmB.lb*EK.data$A + KmA.lb*EK.data$B + EK.data$A*EK.data$B)
  } else {
    KmA.lb = F
    KmB.lb = F
    Vmax.lb = F
    KmA.ub = F
    KmB.ub = F
    Vmax.ub = F
  }

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
    V0.inv.B = (KmB*A.range + KmA*B.conc + A.range*B.conc) /
      (Vmax*A.range*B.conc)
    start.pos = 1 + num.A*counter                                             # starting index
    end.pos = num.A*(1 + counter)                                             # ending index
    A.LWB.df[start.pos:end.pos, "V0.inv"] = V0.inv.B                          # place data in the V0.inv column
    counter = counter + 1                                                     # increment counter
  }

  # B as range
  B.LWB.df = data.frame(A = as.factor(A.seqB),            # dataframe for 1/A vs 1/V0
                        B = B.seqB,
                        B.inv = 1/B.range,
                        V0.inv = NA)
  counter = 0
  for (A.conc in A.concs) {
    V0.inv.B = (KmB*A.conc + KmA*B.range + A.conc*B.range) /
      (Vmax*A.conc*B.range)
    start.pos = 1 + num.B*counter                                               # starting index
    end.pos = num.B*(1 + counter)                                               # ending index
    B.LWB.df[start.pos:end.pos, "V0.inv"] = V0.inv.B                            # place data in the V0.inv column
    counter = counter + 1                                                       # increment counter
  }


  print("Lineweaver-Burk performed")


  # Residuals
  EK.data$Resids = EK.data$V0 - EK.data$V0.fit


  print("Residuals calculated")




  ## Results ----
  cat(sprintf("Km%s is %.3f, \nKm%s is %.3f, \nVmax is %.3f\n", name.1, KmA, name.2, KmB, Vmax)) # print a statement about results, extend as necessary


  # Figure 1 - enzyme kinetics, substrate one
  fig1.params = list(Km = KmA, Km.lb = KmA.lb, Km.ub = KmA.ub,
                     Vmax = Vmax, Vmax.lb = Vmax.lb, Vmax.ub = Vmax.ub,
                     name = name.1)
  fig1.labs = list(x.lab1, y.lab)
  enz.plot.A = enzKinet2::Directplot(EK.data, A.fit.df, fig1.params, fig1.labs, title.1, "A", x.lab2, plot.mods)

  # Figure 2 - Lineweaver-Burk, substrate one
  fig2.labs = list(x.lab1.inv, y.lab.inv)
  LWB.plot.A = enzKinet2::LWBplot(EK.data, A.LWB.df, fig2.labs, title.2, "A", x.lab2)

  # Figure 3 - enzyme kinetics, substrate two
  fig3.params = list(Km = KmB, Km.lb = KmB.lb, Km.ub = KmB.ub,
                     Vmax = Vmax, Vmax.lb = Vmax.lb, Vmax.ub = Vmax.ub,
                     name = name.1)
  fig3.labs = list(x.lab2, y.lab)
  enz.plot.B = enzKinet2::Directplot(EK.data, B.fit.df, fig3.params, fig3.labs, title.1, "B", x.lab1, plot.mods)

  # Figure 4 - Lineweaver-Burk, substrate two
  fig4.labs = list(x.lab2.inv, y.lab.inv)
  LWB.plot.B = enzKinet2::LWBplot(EK.data, B.LWB.df, fig4.labs, title.2, "B", x.lab1)

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
  standard.error = unname(summary(model)$coefficients[,2])
  print(summary(model)$coefficients[,2])
  R2 = modelr::rsquare(model,EK.data)
  RMSE = modelr::rmse(model, EK.data)
  MAE = modelr::mae(model, EK.data)
  Glance = broom::glance(model)
  stats = list(Model = "PP",
               KmA = KmA,
               KmA.err = standard.error[1],
               KmB = KmB,
               KmB.err = standard.error[2],
               KI = NA,
               KI.err = NA,
               Ksat = NA,
               Ksat.err = NA,
               h = NA,
               h.err = NA,
               Vmax = Vmax,
               Vmax.err = standard.error[3],
               Ksi = NA,
               Ksi.se = NA,
               R2 = R2,
               RMSE = RMSE,
               MAE = MAE,
               AIC = Glance$AIC,
               BIC = Glance$BIC,
               logLik = Glance$logLik)


  # Return parameters
  if (conf.level != 0) {
    return(list(KmA,KmB,Vmax,enz.plot.A,enz.plot.B,LWB.plot.A,LWB.plot.B,res.plot,stats,A.fit.df,B.fit.df,confints))
  } else {
    return(list(KmA,KmB,Vmax,enz.plot.A,enz.plot.B,LWB.plot.A,LWB.plot.B,res.plot,stats,A.fit.df,B.fit.df, 0))
  }
}
