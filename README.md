# enzKinet2

enzKinet2 is an R package useful for the analysis of enzyme kinetics. It includes functions for calculating and plotting various models, including but not limited to:

- Michaelis-Menten
- Competitive inhibition
- Mixed inhibition
- Ternary-complex
- Ping-pong


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

# Pick a model, define input file and plot options
model.name = "MM"
input.path = "path/to/data.csv"
plot.options = list(title.1 = "Enzyme Kinetics \nDirect plot",
                  title.2 = "Enzyme Kinetics \nLineweaver-Burk",
                  x.units = "",
                  y.units = "",
                  plot.mods = "",
                  options = 1)
conf.level = 0.95

# Run the model
params = enzKinet2::EK.main(input.path, model.name, plot.options, conf.level)

# Print the parameter outputs
print(params)
```


## Enzyme Kinetics Analysis (EKA)
Enzyme Kinetics Analysis (EKA) is a web application that provides an easy to use inferface for the enzKinet2 package. EKA can be accessed [here](https://enzyme-kinetics.shinyapps.io/enzkinet_webpage/).
#### Academic Paper
For more detailed information on EKA, you can refer to our academic [paper](https://iubmb.onlinelibrary.wiley.com/doi/10.1002/bmb.21823).


## Contact
For more information, please contact:  
   **Email**:   haig.bishop@gp.canterbury.ac.nz  
   **GitHub**:  HaigBishop  
