# constants.R

# Define valid models constant
#' @export
VALID_MODELS <- c("MM", "MMSI", "OGMM", "ALTMM", "CI", "UCI", "NCI", "MI", "TC", "HILL", "PP", "SBK", "CBK")

# Define all parameters that are used
#' @export
ALL_PARAMETERS <- c("Km", "KmA", "KmB", "Ksi", "Ki", "Kic", "Kiu", "Ksat", "Vmax", "Hill", "KD", "Kcat", "E0", "KA")

# Define all extra independent variables that are used (extra on top of the primary ones like A and FB)
#' @export
ALL_EXTRA_INDEPENDENT_VARS <- c("I", "B", "R")

# Define a dictionary for model parameters
#' @export
MODEL_PARAMETERS <- list(
    MM = c("Km", "Vmax"),
    MMSI = c("Km", "Vmax", "Ksi"),
    OGMM = c("Km", "Kcat", "E0"),
    ALTMM = c("KA", "Kcat", "E0"),
    CI = c("Km", "Vmax", "Ki"),
    UCI = c("Km", "Vmax", "Ki"),
    NCI = c("Km", "Vmax", "Ki"),
    MI = c("Km", "Vmax", "Kic", "Kiu"),
    TC = c("KmA", "Vmax", "KmB", "Ksat"),
    HILL = c("Km", "Vmax", "Hill"),
    PP = c("KmA", "Vmax", "KmB"),
    SBK = c("KD"),
    CBK = c("KD")
)

# Define a dictionary for model variables
#' @export
MODEL_VARIABLES <- list(
    # Dependent variable always second
    # Primary independent variable always first
    MM = c("A", "V"),
    MMSI = c("A", "V"),
    OGMM = c("A", "V"),
    ALTMM = c("A", "V"),
    CI = c("A", "V", "I"),
    UCI = c("A", "V", "I"),
    NCI = c("A", "V", "I"),
    MI = c("A", "V", "I"),
    TC = c("A", "V", "B"),
    HILL = c("A", "V"),
    PP = c("A", "V", "B"),
    SBK = c("P", "FB"),
    CBK = c("P", "FB", "R")
)

# Define a dictionary for model dependent variable domains
#' @export
MODEL_DEPENDENT_VAR_DOMAINS <- list(
    MM = c(0, Inf),
    MMSI = c(0, Inf),
    OGMM = c(0, Inf),
    ALTMM = c(0, Inf),
    CI = c(0, Inf),
    UCI = c(0, Inf),
    NCI = c(0, Inf),
    MI = c(0, Inf),
    TC = c(0, Inf),
    HILL = c(0, Inf),
    PP = c(0, Inf),
    SBK = c(0, 1),
    CBK = c(0, 1)
)

# Define a dictionary for model options
#' @export
MODEL_OPTIONS <- c(
    "Michaelis-Menten" = "MM",
    "Michaelis-Menten with Substrate Inhibition" = "MMSI",
    "Original Michaelis-Menten" = "OGMM",
    "Alternative Michaelis-Menten" = "ALTMM",
    "Competitive Inhibition" = "CI",
    "Uncompetitive Inhibition" = "UCI",
    "Non-competitive Inhibition" = "NCI",
    "Mixed Inhibition" = "MI",
    "Ternary Complex" = "TC",
    "Hill" = "HILL",
    "Ping-Pong" = "PP",
    "Simple Binding Kinetics" = "SBK",
    "Complex Binding Kinetics" = "CBK"
)

# Define a dictionary for plot titles
#' @export
PLOT_TITLES <- list(
    MM = "Michaelis-Menten",
    MMSI = "Michaelis-Menten with Substrate Inhibition",
    OGMM = "Original Michaelis-Menten",
    ALTMM = "Alternative Michaelis-Menten",
    CI = "Competitive Inhibition",
    UCI = "Uncompetitive Inhibition",
    NCI = "Non-competitive Inhibition",
    MI = "Mixed Inhibition",
    TC = "Ternary Complex",
    HILL = "Hill",
    PP = "Ping-Pong",
    SBK = "Simple Binding Kinetics",
    CBK = "Complex Binding Kinetics"
)

# Define a dictionary for plot axis titles
#' @export
AXIS_TITLES <- list(
    V = "Velocity V",
    A = "Substrate Concentration [A]",
    FB = "Fraction Bound FB",
    P = "Concentration of Binding Partner in Excess [P]",
    R = "Concentration of Trace Limiting Partner [R]"
)

# Define a dictionary for model parameter strings
#' @export
MODEL_PARAMETER_STRINGS <- list(
    MM = "Km, Vmax",
    MMSI = "Km, Vmax, and Ksi",
    OGMM = "Km, Kcat, and E0",
    ALTMM = "KA, Kcat, and E0",
    CI = "Km, Vmax, and Ki",
    UCI = "Km, Vmax, and Ki",
    NCI = "Km, Vmax, and Ki",
    MI = "Km, Vmax, Kic, and Kiu",
    TC = "KmA, Vmax, KmB and Ksat",
    HILL = "Km, Vmax and Hill",
    PP = "KmA, Vmax and KmB",
    SBK = "KD",
    CBK = "KD"
)

# Define a dictionary for model variable strings
#' @export
MODEL_VARIABLE_STRINGS <- list(
    MM = "A and V",
    MMSI = "A and V",
    OGMM = "A and V",
    ALTMM = "A and V",
    CI = "A, V and I",
    UCI = "A, V and I",
    NCI = "A, V and I",
    MI = "A, V and I",
    TC = "A, V and B",
    HILL = "A and V",
    PP = "A, V and B",
    SBK = "P and FB",
    CBK = "P, FB and R"
)

# Define a dictionary for model formulae
MODEL_FORMULAE <- list(
    MM = formula(V ~ Vmax * A / (Km + A)),
    MMSI = formula(V ~ Vmax * A / (Km + A + A * A / Ksi)),
    OGMM = formula(V ~ Kcat * E0 * A / (Km + A)),
    ALTMM = formula(V ~ Kcat * KA * E0 * A / (Kcat + KA * A)),
    CI = formula(V ~ Vmax * A / (Km * (1 + I / Ki) + A)),
    UCI = formula(V ~ Vmax * A / (Km + A * (1 + I / Ki))),
    NCI = formula(V ~ Vmax * A / ((1 + I / Ki) * (Km + A))),
    MI = formula(V ~ Vmax * A / (Km * (1 + I / Kic) + A * (1 + I / Kiu))),
    TC = formula(V ~ (Vmax * A * B / (KmB * A + KmA * B + A * B + Ksat * KmB))),
    HILL = formula(V ~ Vmax * (A^Hill) / (Km^Hill + A^Hill)),
    PP = formula(V ~ (Vmax * A * B / (KmA * B + KmB * A + A * B))),
    SBK = formula(FB ~ (P / (P + KD))),
    CBK = formula(FB ~ ((R + P + KD) - sqrt((R + P + KD)^2 - 4 * R * P)) / (2 * R))
)


# Define a dictionary for model formulae for display
#' @export
MODEL_FORMULAE_DISPLAY <- list(
    MM = "V = \\frac{V_{max} \\cdot A}{K_m + A}",
    MMSI = "V = \\frac{V_{max} \\cdot A}{K_m + A + \\frac{A^2}{K_{si}}}",
    OGMM = "V = \\frac{K_{cat} \\cdot E_{0} \\cdot A}{K_m + A}",
    ALTMM = "V = \\frac{K_{cat} \\cdot K_{A} \\cdot E_{0} \\cdot A}{K_{cat} + K_{A} \\cdot A}",
    CI = "V = \\frac{V_{max} \\cdot A}{K_m \\left(1 + \\frac{I}{K_i}\\right) + A}",
    UCI = "V = \\frac{V_{max} \\cdot A}{K_m + A \\left(1 + \\frac{I}{K_i}\\right)}",
    NCI = "V = \\frac{V_{max} \\cdot A}{\\left(1 + \\frac{I}{K_i}\\right)(K_m + A)}",
    MI = "V = \\frac{V_{max} \\cdot A}{K_m \\left(1 + \\frac{I}{K_{ic}}\\right) + A \\left(1 + \\frac{I}{K_{iu}}\\right)}",
    TC = "V = \\frac{V_{max} \\cdot A \\cdot B}{K_{mB} \\cdot A + K_{mA} \\cdot B + A \\cdot B + K_{sat} \\cdot K_{mB}}",
    HILL = "V = V_{max} \\cdot \\frac{A^{Hill}}{K_m^{Hill} + A^{Hill}}",
    PP = "V = \\frac{V_{max} \\cdot A \\cdot B}{K_{mA} \\cdot B + K_{mB} \\cdot A + A \\cdot B}",
    SBK = "F_B = \\frac{P}{P + K_D}",
    CBK = "F_B = \\frac{(R + P + K_D) - \\sqrt{(R + P + K_D)^2 - 4 \\cdot R \\cdot P}}{2 \\cdot R}"
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
    OGMM = function(params, A.range, z.range) {
        # Extract parameters
        Km <- params$Km
        Kcat <- params$Kcat
        E0 <- params$E0
        # Create a data frame with all combinations of A.range
        grid <- expand.grid(A = A.range)
        # Calculate V for each combination
        grid$V <- Kcat * E0 * grid$A / (Km + grid$A)
        # Return data frame
        return(grid)
    },
    ALTMM = function(params, A.range, z.range) {
        # Extract parameters
        KA <- params$KA
        Kcat <- params$Kcat
        E0 <- params$E0
        # Create a data frame with all combinations of A.range
        grid <- expand.grid(A = A.range)
        # Calculate V for each combination
        grid$V <- Kcat * KA * E0 * grid$A / (Kcat + KA * grid$A)
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
    },
    SBK = function(params, P.range, z.range) {
        # Extract parameters
        KD <- params$KD
        # Create a data frame with all combinations of P.range
        grid <- expand.grid(P = P.range)
        # Calculate FB for each combination
        grid$FB <- grid$P / (grid$P + KD)
        # Return data frame
        return(grid)
    },
    CBK = function(params, P.range, R.range) {
        # Extract parameters
        KD <- params$KD
        # Create a data frame with all combinations of P.range
        grid <- expand.grid(P = P.range, R = R.range)
        # Calculate FB for each combination
        grid$ FB <- ((grid$R + grid$P + KD) - sqrt((grid$R + grid$P + KD)^2 - 4 * grid$R * grid$P)) / (2 * grid$R)
        # Return data frame
        return(grid)
    }
)
