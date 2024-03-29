% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Custom.1S.R
\name{Custom.1S}
\alias{Custom.1S}
\title{Custom One Substrate Model}
\usage{
Custom.1S(EK.data, formula.text)
}
\arguments{
\item{EK.data}{a dataframe containing the enzyme kinetics data- two columns,
the first for substrate concentration and the second for velocity.}

\item{formula.text}{a string defining the formula to be used in the form
"V0 ~ equation"}
}
\value{
list(Km, Vmax, model, stats)
Prints- Km and Vmax
Plots - A vs V0, 1/A vs 1/V0, and A vs residuals
}
\description{
Custom One Substrate Model
}
\author{
Daniel Mak
16/03/2021

This function analyses enzyme kinetics data and reports on the results
}
