% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Michaelis.Menten.SI.R
\name{Michaelis.Menten.SI}
\alias{Michaelis.Menten.SI}
\title{Michaelis-Menten Model with Substrate inhibition}
\usage{
Michaelis.Menten.SI(EK.data, plot.options, conf.level)
}
\arguments{
\item{EK.data}{a dataframe containing the enzyme kinetics data- two columns,
the first for substrate concentration and the second for velocity.}

\item{plot.options}{a list of plot options to use. Must include numeric
variable "options", which decides on the use of the base options
(options = 1) or the custom options (options = 2)}
}
\value{
list(Km, Vmax, Ksi, model, stats)
Prints- Km, Vmax and Ksi
Plots - A vs V0, 1/A vs 1/V0, and A vs residuals
}
\description{
Michaelis-Menten Model with Substrate inhibition
}
\author{
Haig Bishop
23/06/2024

This function analyses enzyme kinetics data and reports on the results
}
