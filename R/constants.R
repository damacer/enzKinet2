# constants.R

# Define valid models constant
VALID_MODELS <- c("MM", "MMSI", "CI", "UCI", "NCI", "MI", "TC", "HILL", "PP")

# Define a dictionary for model parameters
MODEL_PARAMETERS <- list(
    MM = c("Km", "Vmax"),
    MMSI = c("Km", "Vmax", "Ksi"),
    CI = c("Km", "Vmax", "Ki"),
    UCI = c("Km", "Vmax", "Ki"),
    NCI = c("Km", "Vmax", "Ki"),
    MI = c("Km", "Vmax", "Kic", "Kiu"),
    TC = c("KmA", "Vmax", "KmB", "Ksat"),
    HILL = c("Km", "Vmax", "Hill"),
    PP = c("KmA", "Vmax", "KmB")
)

# Define a dictionary for model variables
MODEL_VARIABLES <- list(
    MM = c("A", "V"),
    MMSI = c("A", "V"),
    CI = c("A", "V", "I"),
    UCI = c("A", "V", "I"),
    NCI = c("A", "V", "I"),
    MI = c("A", "V", "I"),
    TC = c("A", "V", "B"),
    HILL = c("A", "V"),
    PP = c("A", "V", "B")
)

# Define a dictionary for plot titles
PLOT_TITLES <- list(
    MM = "Michaelis-Menten",
    MMSI = "Michaelis-Menten with Substrate Inhibition",
    CI = "Competitive Inhibition",
    UCI = "Uncompetitive Inhibition",
    NCI = "Non-competitive Inhibition",
    MI = "Mixed Inhibition",
    TC = "Ternary Complex",
    HILL = "Hill",
    PP = "Ping-Pong"
)

# Define a dictionary for model parameter strings
MODEL_PARAMETER_STRINGS <- list(
    MM = "Km, Vmax",
    MMSI = "Km, Vmax, and Ksi",
    CI = "Km, Vmax, and Ki",
    UCI = "Km, Vmax, and Ki",
    NCI = "Km, Vmax, and Ki",
    MI = "Km, Vmax, Kic, and Kiu",
    TC = "KmA, Vmax, KmB and Ksat",
    HILL = "Km, Vmax and Hill",
    PP = "KmA, Vmax and KmB"
)

# Define a dictionary for model variable strings
MODEL_VARIABLE_STRINGS <- list(
    MM = "A and V",
    MMSI = "A and V",
    CI = "A, V and I",
    UCI = "A, V and I",
    NCI = "A, V and I",
    MI = "A, V and I",
    TC = "A, V and B",
    HILL = "A and V",
    PP = "A, V and B"
)

# Define a dictionary for model formulae
MODEL_FORMULAE <- list(
    MM = formula(V ~ Vmax * A / (Km + A)),
    MMSI = formula(V ~ Vmax * A / (Km + A + A * A / Ksi)),
    CI = formula(V ~ Vmax * A / (Km * (1 + I / Ki) + A)),
    UCI = formula(V ~ Vmax * A / (Km + A * (1 + I / Ki))),
    NCI = formula(V ~ Vmax * A / ((1 + I / Ki) * (Km + A))),
    MI = formula(V ~ Vmax * A / (Km * (1 + I / Kic) + A * (1 + I / Kiu))),
    TC = formula(V ~ (Vmax * A * B / (KmB * A + KmA * B + A * B + Ksat * KmB))),
    HILL = formula(V ~ Vmax * (A^Hill) / (Km^Hill + A^Hill)),
    PP = formula(V ~ (Vmax * A * B / (KmA * B + KmB * A + A * B)))
)

# Define model functions (takes parameters and independent variables - output perfect data)
MODEL_FUNCTIONS <- list(
    MM = function(params, A.range, z.range) {
        # Extract parameters
        Km <- params$Km
        Vmax <- params$Vmax
        # Create a data frame with all combinations of A.range
        grid <- expand.grid(A = A.range)
        # Calculate V for each combination
        grid$V <- Vmax * grid$A / (Km + grid$A)
        # Return data frame
        return(grid)
    },
    MMSI = function(params, A.range, z.range) {
        # Extract parameters
        Km <- params$Km
        Vmax <- params$Vmax
        Ksi <- params$Ksi
        # Create a data frame with all combinations of A.range
        grid <- expand.grid(A = A.range)
        # Calculate V for each combination
        grid$V <- Vmax * grid$A / (Km + grid$A + (grid$A * grid$A) / Ksi)
        # Return data frame
        return(grid)
    },
    CI = function(params, A.range, I.range) {
        # Extract parameters
        Km <- params$Km
        Vmax <- params$Vmax
        Ki <- params$Ki
        # Create a data frame with all combinations of A.range and I.range
        grid <- expand.grid(A = A.range, I = I.range)
        # Calculate V for each combination
        grid$V <- Vmax * grid$A / (Km * (1 + grid$I / Ki) + grid$A)
        # Return data frame
        return(grid)
    },
    UCI = function(params, A.range, I.range) {
        # Extract parameters
        Km <- params$Km
        Vmax <- params$Vmax
        Ki <- params$Ki
        # Create a data frame with all combinations of A.range and I.range
        grid <- expand.grid(A = A.range, I = I.range)
        # Calculate V for each combination
        grid$V <- Vmax * grid$A / (Km + grid$A * (1 + grid$I / Ki))
        # Return data frame
        return(grid)
    },
    NCI = function(params, A.range, I.range) {
        # Extract parameters
        Km <- params$Km
        Vmax <- params$Vmax
        Ki <- params$Ki
        # Create a data frame with all combinations of A.range and I.range
        grid <- expand.grid(A = A.range, I = I.range)
        # Calculate V for each combination
        grid$V <- Vmax * grid$A / ((1 + grid$I / Ki) * (Km + grid$A))
        # Return data frame
        return(grid)
    },
    MI = function(params, A.range, I.range) {
        # Extract parameters
        Km <- params$Km
        Vmax <- params$Vmax
        Kic <- params$Kic
        Kiu <- params$Kiu
        # Create a data frame with all combinations of A.range and I.range
        grid <- expand.grid(A = A.range, I = I.range)
        # Calculate V for each combination
        grid$V <- Vmax * grid$A / (Km * (1 + grid$I / Kic) + grid$A * (1 + grid$I / Kiu))
        # Return data frame
        return(grid)
    },
    TC = function(params, A.range, B.range) {
        # Extract parameters
        KmA <- params$KmA
        KmB <- params$KmB
        Vmax <- params$Vmax
        Ksat <- params$Ksat
        # Create a data frame with all combinations of A.range and B.range
        grid <- expand.grid(A = A.range, B = B.range)
        # Calculate V for each combination
        grid$V <- Vmax * grid$A * grid$B / (KmB * grid$A + KmA * grid$B + grid$A * grid$B + Ksat * KmB)
        # Return data frame
        return(grid)
    },
    HILL = function(params, A.range, z.range) {
        # Extract parameters
        Km <- params$Km
        Vmax <- params$Vmax
        Hill <- params$Hill
        # Create a data frame with all combinations of A.range
        grid <- expand.grid(A = A.range)
        # Calculate V for each combination
        grid$V <- Vmax * (grid$A^Hill) / (Km^Hill + grid$A^Hill)
        # Return data frame
        return(grid)
    },
    PP = function(params, A.range, B.range) {
        # Extract parameters
        KmA <- params$KmA
        KmB <- params$KmB
        Vmax <- params$Vmax
        # Create a data frame with all combinations of A.range and B.range
        grid <- expand.grid(A = A.range, B = B.range)
        # Calculate V for each combination
        grid$V <- Vmax * grid$A * grid$B / (KmA * grid$B + KmB * grid$A + grid$A * grid$B)
        # Return data frame
        return(grid)
    }
)