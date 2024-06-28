# constants.R

# Define valid models constant
VALID_MODELS <- c("MM", "MMSI")

# Define a dictionary for model parameters
MODEL_PARAMETERS <- list(
    MM = c("KmA", "Vmax"),
    MMSI = c("KmA", "Vmax", "Ksi")
)

# Define a dictionary for plot titles
PLOT_TITLES <- list(
    MM = "Michaelis-Menten",
    MMSI = "Michaelis-Menten with Substrate Inhibition"
)

# Define a dictionary for model parameter descriptions
MODEL_PARAMETER_STRINGS <- list(
    MM = "KmA, Vmax",
    MMSI = "KmA, Vmax, and Ksi"
)

# Define a dictionary for model formulae
MODEL_FORMULAE <- list(
    MM = formula(V0 ~ Vmax * A / (KmA + A)),
    MMSI = formula(V0 ~ Vmax * A / (KmA + A + (A * A) / Ksi))
)

# Define model functions
MODEL_FUNCTIONS <- list(
    MM = function(params, x.range) {
        KmA <- params$KmA
        Vmax <- params$Vmax
        data.frame(A = x.range, V0 = Vmax * x.range / (KmA + x.range))
    },
    MMSI = function(params, x.range) {
        KmA <- params$KmA
        Vmax <- params$Vmax
        Ksi <- params$Ksi
        data.frame(A = x.range, V0 = Vmax * x.range / (KmA + x.range + (x.range * x.range) / Ksi))
    }
)