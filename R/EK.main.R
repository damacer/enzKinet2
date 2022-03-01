## Control function for enzKinet2 package

#' Enzyme Kinetics for Website Implementation
#'
#' @author Daniel Mak
#' 17/03/2021
#'
#' This function provides top-level control for the enzKinet package
#' @param file a .csv file containing enzyme kinetics data
#' @param model the name of the model to be used
#' @param plot.options a list containing plot options. Must include options = 1
#' or 2.
#' @return
#'
#' @export

EK.main = function(file, model,plot.options, conf.level) {
  print("Starting EK.main")

  # Read file
  EK.data = read.csv(file, fileEncoding = "UTF-8-BOM")


  # Run Analysis
  if (model == "MM") {
    params = enzKinet2::Michaelis.Menten(EK.data,plot.options, conf.level)


  } else if (model == "LCI") {
    params = enzKinet2::LCI(EK.data,plot.options, conf.level)


  } else if (model == "LMI") {
    params = enzKinet2::LMI(EK.data,plot.options, conf.level)


  } else if (model == "LUCI") {
    params = enzKinet2::LUCI(EK.data,plot.options, conf.level)


  } else if (model == "LNCI") {
    params = enzKinet2::LNCI(EK.data,plot.options, conf.level)


  } else if (model == "TC") {
    params = enzKinet2::Ternary.complex(EK.data,plot.options, conf.level)


  } else if (model == "PP") {
    params = enzKinet2::Ping.pong(EK.data,plot.options, conf.level)


  } else if (model == "Hill") {
    params = enzKinet2::Hill(EK.data, plot.options, conf.level)


  } else {
    stop("Model not recognised")
  }

  return(params)
}
