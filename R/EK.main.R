## Control function for enzKinet2 package

#' Enzyme Kinetics for Website Implementation
#'
#' @author Daniel Mak
#' 17/03/2021
#'
#' This function provides top-level control for the enzKinet package
#' @param file a .csv file containing enzyme kinetics data
#' @param model the name of the model to be used
#' @param units a list containing a pair of strings of the x and y axes units
#' @return
#'
#' @export

EK.main = function(file, model,plot.options) {
  # Read file
  EK.data = read.csv(file, fileEncoding = "UTF-8-BOM")


  # Run Analysis
  if (model == "MM") {
    params = enzKinet2::Michaelis.Menten(EK.data,plot.options)


  } else if (model == "LCI") {
    params = enzKinet2::LCI(EK.data,units)


  } else if (model == "TC") {
    params = enzKinet2::Ternary.complex(EK.data,plot.options)


  } else if (model == "PP") {
    params = enzKinet2::Ping.pong(EK.data,plot.options)


  }else {
    stop("Model not recognised")
  }

  return(params)
}
