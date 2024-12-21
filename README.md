
# enzKinet2 Version 2.0

enzKinet2 is an R package used for the analysis of enzyme kinetics. It's
primary functions are: 1) fitting models to kinetics data, 2) simulating
kinetics data, and 3) plotting these data along with curves.

**The package supports a range of kinetics models, including:** -
Michaelis-Menten (MM) - Michaelis-Menten with Substrate Inhibition
(MMSI) - Original Michaelis-Menten (OGMM) - Alternative Michaelis-Menten
(ALTMM) - Competitive Inhibition (CI) - Uncompetitive Inhibition (UCI) -
Non-competitive Inhibition (NCI) - Mixed Inhibition (MI) - Ternary
Complex (TC) - Hill (HILL) - Ping-Pong (PP) - Binding Kinetics (BK) -
Quadratic Binding Kinetics (QBK)

## Installation

To install the enzKinet2 package, you can clone the repository and
install it using devtools.

1.  Clone the repository (CLI)

    `git clone https://github.com/damacer/enzKinet2.git`

2.  Install the R package (R)

    `devtools::install("enzKinet2")`

## Usage

Once installed, you can load the package and start using its functions
to analyze enzyme kinetics data. 

#### Example usage

``` r

# Import the package
library(enzKinet2)

# Define some parameters for the Michaelis-Menten model
params <- list(Km = 0.5, Vmax = 2.0)

# Define the range of x (subtrate concentration)
x.min <- 0
x.max <- 10

# Generate a curve for the model
curve.df <- make_curve(model = "MM", params = params, x.min = x.min, x.max = x.max)

# Generate synthetic data for the model with some noise
data.df <- simulate_data(model = "MM", params = params, x.min = x.min, x.max = x.max)

# Create a plot of both the curve and the synthetic data
plot <- make_plot(model = "MM", data.df = data.df, curve.df = curve.df)

# Show the plot (Michaelis-Menten curve with synthetic data)
print(plot)

# Transform the synthetic data, so the model no longer fits it
new.data.df <- data.df
new.data.df$V <- new.data.df$V * 1.2

# Create a plot to see how the model fits the new data
plot2 <- make_plot(model = "MM", data.df = new.data.df, curve.df = curve.df)

# Show the plot
print(plot2)

# Fit the model to the new data 
results <- fit_model(model = "MM", data.df = new.data.df, start.params = params)
fitted.params <- results$fitted.params

# Print the fitted params
print(paste("Fitted Km:", sprintf("%.4f", fitted.params$Km)))
print(paste("Fitted Vmax:", sprintf("%.4f", fitted.params$Vmax)))

# Generate a curve to visualise the new model
fitted.curve.df <- make_curve(model = "MM", params = fitted.params, x.min = x.min, x.max = x.max)

# Create a plot to visualise how the new model fits the new data
plot3 <- make_plot(model = "MM", data.df = new.data.df, curve.df = fitted.curve.df)

# Show the plot
print(plot3)
```

## Functions

### `simulate_data`
Generates synthetic enzyme kinetics data for a specified model.

**Parameters**  
- **model**: Character string indicating the model ("MM", "CI", etc.).  
- **params**: List of parameter values for the model (e.g. Km, Vmax).  
- **x.min, x.max**: Numeric ranges to generate the primary independent variable.  
- **z.values**: Optional numeric vector for models with a second independent variable.  
- **n.samples**: Positive integer for how many unique x values to generate.  
- **n.replicates**: Positive integer for replicates of those x values.  
- **noise.level**: Numeric; 0 for no noise, or a positive value.  
- **noise.type**: `"absolute"` or `"relative"` noise.  
- **space**: Distribution of x values (`"linear"`, `"exponential"`, `"dilution.series"`, etc.).  
- **dilution.factor**: Factor for serial dilutions if using `space = "dilution.series"`.  
- **x.range**: Custom x values if using `space = "custom"`.  
- **distribution**: Noise distribution (`"norm"` or `"truncated.norm"`).  

**Returns**  
A data frame containing the generated data.  

---

### `fit_model`
Fits a model to observed data and optionally returns confidence intervals and statistics.

**Parameters**  
- **model**: Character string of the model to fit ("MM", "CI", etc.).  
- **data.df**: Data frame containing the variables required by the chosen model.  
- **start.params**: Starting values for parameters (list).  
- **fit.method**: Fitting approach (`"nls"`, `"recursive"`, `"ss.calc"`, `"nonparametric"`).  
- **locked.params**: Vector of parameter names to hold constant (only for `"nls"`).  
- **add.minor.noise**: If `TRUE`, adds a tiny amount of noise prior to fitting.  
- **override.data.point.check**: If `TRUE`, bypasses data-point quantity checks.  
- **get.conf.int**: If `TRUE`, attempts to compute parameter confidence intervals.  
- **conf.level**: Confidence level (0 < value < 1).  
- **get.stats**: If `TRUE`, returns model fit statistics (R², AIC, BIC, etc.).  

**Returns**  
A list containing:  
- **fitted.params**: Estimated parameter values or `NULL` if fitting failed.  
- **statistics**: Fit statistics if requested (or `NULL` otherwise).  
- **error**: A character message if fitting failed.  

---

### `make_curve`
Generates a noiseless curve (dense points) for a specified model and parameter set.

**Parameters**  
- **model**: Model name ("MM", "CI", etc.).  
- **params**: Named list of parameter values (e.g. Km, Vmax).  
- **x.min, x.max**: Numeric range for the primary independent variable.  
- **z.values**: Optional numeric vector for a second independent variable.  
- **n.samples**: Resolution (number of x points) for the curve.  
- **space**: Distribution of x values (`"linear"`, `"exponential"`, etc.).  
- **conf.int**: If `TRUE`, uses parameter bounds (e.g. `.lb`, `.ub`) to produce bounding curves.  

**Returns**  
A data frame of noiseless data for the chosen model, optionally including lower and upper bound columns if `conf.int` is `TRUE`.  

---

### `make_plot`
Creates a ggplot-based enzyme kinetics plot for data and/or curves.

**Parameters**  
- **model**: Model name ("MM", "CI", etc.).  
- **data.df**: Data frame to be plotted (optional).  
- **curve.df**: Data frame for the model curve (optional).  
- **extra.curve**: An additional curve (optional).  
- **plot.transformation**: Plot transformation (`"standard"`, `"lineweaver"`, `"direct"`, etc.).  
- **conf.int**: If `TRUE`, plots confidence interval ribbons using `.lb` and `.ub` columns.  
- **x.label, y.label**: Custom axis labels (strings).  
- **x.units, y.units**: Units to appear next to axis labels.  
- **title**: Custom title.  
- **legend.label**: Title of the legend.  
- **palette**: Colour palette (e.g. `"Set1"`).  
- **hide.legend**: If `TRUE`, hides the legend.  

**Returns**  
A `ggplot` object showing the desired enzyme kinetics plot.  

---

### `make_residual_plot`
Creates a residual plot to help evaluate how well the model fits the data.

**Parameters**  
- **model**: Model name ("MM", "CI", etc.).  
- **params**: Parameter list used to compute the fitted model.  
- **data.df**: Data to calculate residuals against the model.  
- **x.max**: Maximum value to display on the x-axis.  
- **x.label, y.label**: Custom axis labels (strings).  
- **x.units, y.units**: Units to appear next to axis labels.  
- **title**: Custom title.  
- **zero.line**: If `TRUE`, draws a line at y=0.  
- **legend.label**: Legend title.  
- **palette**: Colour palette (e.g. `"Set1"`).  
- **hide.legend**: If `TRUE`, hides the legend.  

**Returns**  
A `ggplot` object of residuals vs. the independent variable.  

---

### `update_plot_curve`
Replaces or updates the curve in an existing kinetics plot.

**Parameters**  
- **model**: Model name ("MM", "MMSI", etc.).  
- **plot**: The existing plot (a `ggplot` object).  
- **curve.df**: Data frame for the new curve (optional).  
- **extra.curve**: Additional curve to overlay (optional).  
- **plot.transformation**: Plot transformation (`"standard"`, `"lineweaver"`, `"direct"`, etc.).  
- **conf.int**: If `TRUE`, plots confidence intervals from curve.df.  

**Returns**  
The updated `ggplot` object.

## Enzyme Kinetics Analysis (EKA)
Enzyme Kinetics Analysis (EKA) is a web application that provides an intuitive inferface for the enzKinet2 package. EKA can be accessed [here](https://enzyme-kinetics.shinyapps.io/enzkinet_webpage/). 

#### Academic Paper
For more detailed information on EKA, you can refer to our academic [paper](https://iubmb.onlinelibrary.wiley.com/doi/10.1002/bmb.21823).

## Contact
For more information, please contact:

**Email**: [haig.bishop\@pg.canterbury.ac.nz](mailto:haig.bishop@pg.canterbury.ac.nz)

**GitHub**: HaigBishop
