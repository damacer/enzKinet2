# enzKinet2

enzKinet2 is an R package useful for the analysis of enzyme kinetics. This is version 2 of the package, which to begin with is just a refactor, but in future will include more features. It includes functions for calculating and plotting various models, including:

- Michaelis-Menten (MM)
- Michaelis-Menten with Substrate Inhibition (MMSI)
- Original Michaelis-Menten (OGMM)
- Alternative Michaelis-Menten (ALTMM)
- Competitive Inhibition (CI)
- Uncompetitive Inhibition (UCI)
- Non-competitive Inhibition (NCI)
- Mixed Inhibition (MI)
- Ternary Complex (TC)
- Hill (HILL)
- Ping-Pong (PP)
- Binding Kinetics (BK)
- Quadratic Binding Kinetics (QBK)

## Installation

To install the enzKinet2 package, you can clone the repository and install it using devtools.

1. Clone the repository (CLI)  
   `git clone https://github.com/damacer/enzKinet2.git`

2. Install the R package (R)  
   `devtools::install("enzKinet2")`


## Usage

Once installed, you can load the package and start using its functions to analyze enzyme kinetics data.
#### Example usage

```R
# Import the package
library(enzKinet2)

# Define some parameters for the Michaelis-Menten model
params <- list(Km = 0.5, Vmax = 2.0)

# Define the range of x
x.min <- 0
x.max <- 10

# Generate a curve for the model
curve.df <- make_curve(model = "MM", params = params, x.min = x.min, x.max = x.max)

# Generate synthetic data for the model with some noise
synthetic.data <- simulate_data(model = "MM", params = params, x.min = x.min, 
                                x.max = x.max, noise_level = 0.05)

# Create a plot of both the curve and the synthetic data
plot <- make_plot(model = "MM", data.df = synthetic.data, curve.df = curve.df)

# Show the plot
print(plot)

# Transform the synthetic data, so the model no longer fits it
synthetic.data$V <- synthetic.data$V * 1.2

# Create a plot to see how the model fits the new data
plot <- make_plot(model = "MM", data.df = synthetic.data, curve.df = curve.df)

# Show the plot
print(plot)

# Fit the model to the new data 
fitted.params <- fit_model(model = "MM", data.df = synthetic.data, start.params = params)

# Generate a curve to visualise the new model
fitted.curve.df <- make_curve(model = "MM", params = fitted.params, x.min = x.min, x.max = x.max)

# Create a plot to see how the new model fits the new data
plot <- make_plot(model = "MM", data.df = synthetic.data, curve.df = fitted.curve.df)

# Show the plot
print(plot)

```


## Enzyme Kinetics Analysis (EKA)
Enzyme Kinetics Analysis (EKA) is a web application that provides an easy to use inferface for the enzKinet2 package. EKA can be accessed [here](https://enzyme-kinetics.shinyapps.io/enzkinet_webpage/).
#### Academic Paper
For more detailed information on EKA, you can refer to our academic [paper](https://iubmb.onlinelibrary.wiley.com/doi/10.1002/bmb.21823).


## EnzKinet2 Version 1
The enzKinet2 package had a previous version. This previous version is found in the GitHub repo. It functioned differently this version of enzKinet2, and was suited to work with the current version of the EKA website, which will be upgraded later this year.


## Contact
For more information, please contact:  
   **Email**:   haig.bishop@gp.canterbury.ac.nz  
   **GitHub**:  HaigBishop  
