## TEMPLATE2S function for enzKinet2 package
#' To use this template, replace any words/variables where you see all capitals
#' i.e. "TEMPLATE" above and below this paragraph.
#' Additionally, {} represents optional words/variables to include depending on
#' the model you are using.

#' TEMPLATE2S model
#'
#' @author Daniel Mak
#' 20/04/2021
#'
#' This function analyses enzyme kinetics and reports on the results, using the
#' 3S-TEMPLATE mechanism model.
#'
#' @param EK.data a data-frame containing the enzyme kinetics data- NUMBER
#' columns, the first and second for for substrate concentrations and the
#' third for velocity.
#'
#' @param plot.options a list of plot options to use. Must include the numeric
#' variable "options", which decides on the use of base options (options = 1) or
#' custom options (options = 2).
#'
#' @return list(KmA, KmB, {Ksat, }Vmax, model, stats)
#' Prints- KmA, KmB, {Ksat, }Vmax
#' Plots- A vs Vmax, 1/A vs 1/Vmax, B vs Vmax, 1/B vs 1/Vmax, residuals
#'
#' @export

TEMPLATE2S = function(EK.data, plot.options) {
  print("Starting TEMPLATE")


  ## Setup ----
  # Standardise column names
  data.size = ncol(EK.data)   # find the number of columns in the data
  A.col = 1                   # expect that A data is in the first column
  B.col = 2                   # expect that B data is in the second column
  V0.col = 2                  # expect that V0 data is in the third column

  if (data.size > 3) {                      # if there are more than 3 columns
    if ("V0" %in% colnames(EK.data)) {      # if the header V0 exists in the data
      V0.col = match("V0",names(EK.data))   # change the V0.col number to the correct column
      if (V0.col == 1) {                    # if V0 data is in the first column
        A.col = 2                           # assume that A data is in the second column
        B.col = 3                           # assume that B data is in the third column
      }
    } else {
      V0.col = data.size                  # assume that V0 data is in the last column
    }
  } else if (data.size < 3) {
    return("Error, data requires 3 or more columns")
  }

  name.1 = names(EK.data[A.col])  # store names for later use
  name.2 = names(EK.data[B.col])
  name.3 = names(EK.data[V0.col])

  names(EK.data)[A.col] = "A"     # overwrite names
  names(EK.data)[B.col] = "B"
  names(EK.data)[V0.col] = "V0"


  # Define plot options
  if (plot.options$options == 1) {        # use the base options
    title.1 = "Enzyme Kinetics \nDirect plot"
    title.2 = "Enzyme Kinetics \nLineweaver-Burk"
    x.lab1 = sprintf("%s", name.1)
    x.lab2 = sprintf("%s", name.1)
    y.lab = sprintf("Velocity")
  } else if (plot.options$options == 2) { # use custom options
    title.1 = plot.options$title.1
    title.2 = plot.options$title.2
    x.lab1 = sprintf("%s, %s", name.1, plot.options$x.units)
    x.lab2 = sprintf("%s, %s", name.2, plot.options$x.units)
    y.lab = sprintf("Velocity, %s", plot.options$y.units)
  }


  # Define model
  formu = formula(V0 ~ (Vmax*A*B /
                        (KmA*A + KmB*B + A*B + Ksat*KmB)))


  # Estimate starting parameters
  KmA.est = median(EK.data$A, na.rm = T)  # use median value for KmX values
  KmB.est = median(EK.data$B, na.rm = T)
  Ksat.est = 0.5                          # use 0.5 for Ksat values
  Vmax.est = max(EK.data$V0, na.rm = T)   # use maximum measured value of V0
  ests = list(KmA = KmA.est,
              KmB = KmB.est,
              Ksat = Ksat.est,
              Vmax = Vmax.est)


  # Range for fitted model
  A.low = min(EK.data$A)                              # model lower validity bound
  A.high = max(EK.data$A)                             # model upper validity bound
  A.range = pracma::linspace(A.low, A.high, n = 100)  # model validity range
  A.concs = unique(EK.data$A)                         # list of unique A concs in the experimental data
  num.A = length(A.range)                             # length of the A range vector

  B.low = min(EK.data$B)
  B.high = max(EK.data$B)
  B.range = pracma::linspace(B.low, B.high, n = 100)
  B.concs = unique(EK.data$B)
  num.B = length(B.range)


  # nls control parameters
  nlc = nls.control(maxiter = 1e5, tol = 1e-5)


  print("Setup complete")




  ## Process ----
  # Non-linear least square regression
  model = tryCatch(
    expr = nls(formu, data = EK.data, start = ests, control = nlc),
    error = function(cond) {
      ####
      #### Add additional tryCatch statement to run linear regression if nls failed. This will allow the removal of the necessary noise addition to perfect data
      ####
      print(cond)
      return(c(F,cond))
    }
  )


  # Check if model failed
  if (!is.list(model[[1]])) {
    print("nls failed, returning error statement")
    return(model)
  }

  # Check if data could not be fit !!!! Possibly obsolete code !!!!
  if (!is.list(model)) {
    return("Data could not be fit")
  }


  # Extract parameters
  KmA = unname(coef(model)["KmA"])
  KmB = unname(coef(model)["KmB"])
  Ksat = unname(coef(model)["Ksat"])
  Vmax = unname(coef(model)["Vmax"])


  print("Parameters found")


  # Create data from fitted parameters
  EK.data$V0.fit = Vmax*EK.data$A*EK.data$B /
                   (KmA*EK.data$A + KmB*EK.data$B + EK.data$A*EK.data$B + Ksat*KmB)


  # A as range for each B concentration
  A.seqA = rep(A.range, times = length(num.B))  # vector containing A.range repeated num.B times !!!! WTF? length(num.B)????
  B.seqA = paste(rep(B.concs, each = num.A))    # vector containing each value in B.concs repeated num.A times
  A.fit.df = data.frame(A = A.seqA,
                        B = B.seqA,
                        V0 = NA)                # data-frame for results of the fittefd model using A as the range
  counter = 0

  for (B.conc in B.concs) {
    V0.range = Vmax*A.range*B.conc /
               (KmA*A.range + KmB*B.conc + A.range*B.conc + Ksat*KmB)   # calculate fitted V0
    start.pos = 1 + num.A*counter                                       # starting index
    end.pos = num.A*(1 + counter)                                       # ending index
    A.fit.df[start.pos:end.pos, "V0"] = V0.range                        # place data in the V0 column
    counter = counter + 1                                               # increment counter
  }


  # B as range for each A concentration
  A.seqB = paste(rep(A.concs, each = num.B))
  B.seqB = rep(B.range, times = length(num.A))
  B.fit.df = data.frame(A = A.seqB,
                        B = B.seqB,
                        V0 = NA)
  counter = 0

  for (A.conc in A.concs) {
    V0.range = Vmax*A.conc*B.range /
               (KmA*A.conc + KmB*B.range + A.conc*B.range + Ksat*KmB)
    start.pos = 1 + num.A*counter
    end.pos = num.A*(1 + counter)
    B.fit.df[start.pos:end.pos, "V0"] = V0.range
    counter = counter + 1
  }


  print("Model simulated over range")


  # Invert data for Lineweaver-Burk
  EK.data$A.inv = 1/EK.data$A
  EK.data$B.inv = 1/EK.data$B
  EK.data$V0.inv = 1/EK.data$V0


  # A as range
  A.LWB.df = data.frame(A = A.seqA,
                        B = as.factor(B.seqA),
                        A.inv = 1/A.range,
                        V0.inv = NA)            # data-frame for 1/A vs 1/V0
  counter = 0
  for (B.conc in B.concs) {
    V0.inv.B = (KmA*A.range + KmB*B.conc + A.range*B.conc + Ksat*KmB) /
               (Vmax*A.range*B.conc)
    start.pos = 1 + num.A*counter
    end.pos = num.A*(1 + counter)
    A.LWB.df[start.pos:end.pos, "V0.inv"] = V0.inv.B
    counter = counter + 1
  }


  # B as range
  B.LWB.df = data.frame(A = as.factor(A.seqB),
                        B = B.seqB,
                        B.inv = 1/B.range,
                        V0.inv = NA)
  counter = 0
  for (A.conc in A.concs) {
    V0.inv.B = (KmA*A.conc + KmB*B.range + A.conc*B.range + Ksat*KmB) /
               (Vmax*A.conc*B.range)
    start.pos = 1 + num.B*counter
    end.pos = num.B*(1 + counter)
    B.LWB.df[start.pos:end.pos, "V0.inv"] = V0.inv.B
    counter = counter + 1
  }


  print("Lineweaver-Burk data created")


  # Residuals
  EK.data$Resids = EK.data$V0 - EK.data$V0.fit


  print("Residuals calculated")




  ## Results ----
  cat(sprintf("Km%s is %.3f, \nKm%s is %.3f, \nKsat is %.3f, \nVmax is %.3f\n", name.1, KmA, name.2, KmB, Ksat, Vmax)) # print a statement about results, extend as necessary


  # Figure 1 - enzyme kinetics, substrate one
  fig1.params = list(Km = KmA, Vmax = Vmax, name = name.1)
  fig1.labs = list(x.lab1, y.lab)
  enz.plot.A = enzKinet2::Directplot(EK.data, A.fit.df, fig1.params, fig1.labs, "A")


  # Get stats
  R2 = modelr::rsquare(model, EK.data)
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
  return(list(KmA, KmB, Ksat, Vmax,
              enz.plot.A,
              stats,
              A.fit.df, B.fit.df))
}



