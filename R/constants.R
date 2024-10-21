# constants.R

# All models
#' @export
VALID_MODELS <- c("MM", "MMSI", "OGMM", "ALTMM", "CI", "UCI", "NCI", "MI", "TC", "HILL", "PP", "BK", "QBK")

# All parameters
#' @export
ALL_PARAMETERS <- c("Km", "KmA", "KmB", "Ksi", "Ki", "Kic", "Kiu", "Ksat", "Vmax", "n", "KD", "Kcat", "E0", "KA")

# All extra independent variables (extra on top of the primary ones like A and P)
#' @export
ALL_EXTRA_INDEPENDENT_VARS <- c("I", "B", "R")

# Each model's parameters
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
    HILL = c("Km", "Vmax", "n"),
    PP = c("KmA", "Vmax", "KmB"),
    BK = c("KD"),
    QBK = c("KD")
)

# Each model's variables
#' @export
MODEL_VARIABLES <- list(
    # Primary independent variable always first
    # Dependent variable always second
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
    BK = c("P", "FB"),
    QBK = c("P", "FB", "R")
)

# Each model's dependent variable's domain
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
    BK = c(0, 1),
    QBK = c(0, 1)
)

# All model options
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
    "Binding Kinetics" = "BK",
    "Quadratic Binding Kinetics" = "QBK"
)

# All model options, but in groups
#' @export
MODEL_OPTIONS_GROUPED <- list(
    "Michaelis-Menten Models" = list(
        "Michaelis-Menten" = "MM",
        "Michaelis-Menten with Substrate Inhibition" = "MMSI",
        "Original Michaelis-Menten" = "OGMM",
        "Alternative Michaelis-Menten" = "ALTMM",
        "Hill" = "HILL"),
    "Inhibition Models" = list(
        "Competitive Inhibition" = "CI",
        "Uncompetitive Inhibition" = "UCI",
        "Non-competitive Inhibition" = "NCI",
        "Mixed Inhibition" = "MI"),
    "Multi-Substrate Models" = list(
        "Ternary Complex" = "TC",
        "Ping-Pong" = "PP"),
    "Binding Models" = list(
        "Binding Kinetics" = "BK",
        "Quadratic Binding Kinetics" = "QBK"
    )
)

# Each model's title
#' @export
PLOT_TITLES <- list(
    MM = "Michaelis-Menten",
    MMSI = "Michaelis-Menten with Substrate Inhibition",
    OGMM = "Michaelis-Menten",
    ALTMM = "Michaelis-Menten",
    CI = "Competitive Inhibition",
    UCI = "Uncompetitive Inhibition",
    NCI = "Non-competitive Inhibition",
    MI = "Mixed Inhibition",
    TC = "Ternary Complex",
    HILL = "Hill",
    PP = "Ping-Pong",
    BK = "Binding Kinetics",
    QBK = "Quadratic Binding Kinetics"
)

# Each variable's title
#' @export
AXIS_TITLES <- list(
    V = "Velocity V",
    A = "Substrate A Concentration [A]",
    B = "Substrate B\nConcentration [B]",
    I = "Inhibitor\nConcentration [I]",
    FB = "Fraction Bound FB",
    P = "Concentration of Binding Partner in Excess [P]",
    R = "Concentration of Trace\nLimiting Partner [R]"
)

# Each model's parameters as a string
#' @export
MODEL_PARAMETER_STRINGS <- list(
    MM = "Km and Vmax",
    MMSI = "Km, Vmax, and Ksi",
    OGMM = "Km, Kcat, and E0",
    ALTMM = "KA, Kcat, and E0",
    CI = "Km, Vmax, and Ki",
    UCI = "Km, Vmax, and Ki",
    NCI = "Km, Vmax, and Ki",
    MI = "Km, Vmax, Kic, and Kiu",
    TC = "KmA, Vmax, KmB and Ksat",
    HILL = "Km, Vmax and n",
    PP = "KmA, Vmax and KmB",
    BK = "KD",
    QBK = "KD"
)

# Each model's variables as a string
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
    BK = "P and FB",
    QBK = "P, FB and R"
)

# Each model's formula
#' @export
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
    HILL = formula(V ~ Vmax * (A^n) / (Km^n + A^n)),
    PP = formula(V ~ (Vmax * A * B / (KmA * B + KmB * A + A * B))),
    BK = formula(FB ~ (P / (P + KD))),
    QBK = formula(FB ~ ((R + P + KD) - sqrt((R + P + KD)^2 - 4 * R * P)) / (2 * R))
)

# Each model's formula in LaTeX
#' @export
MODEL_FORMULAE_DISPLAY <- list(
    MM = "\\Large{V = \\frac{V_{max} \\cdot A}{K_m + A}}",
    MMSI = "\\Large{V = \\frac{V_{max} \\cdot A}{K_m + A + \\frac{A^2}{K_{si}}}}",
    OGMM = "\\Large{V = \\frac{K_{cat} \\cdot E_{0} \\cdot A}{K_m + A}}",
    ALTMM = "\\Large{V = \\frac{K_{cat} \\cdot K_{A} \\cdot E_{0} \\cdot A}{K_{cat} + K_{A} \\cdot A}}",
    CI = "\\Large{V = \\frac{V_{max} \\cdot A}{K_m \\left(1 + \\frac{I}{K_i}\\right) + A}}",
    UCI = "\\Large{V = \\frac{V_{max} \\cdot A}{K_m + A \\left(1 + \\frac{I}{K_i}\\right)}}",
    NCI = "\\Large{V = \\frac{V_{max} \\cdot A}{\\left(1 + \\frac{I}{K_i}\\right)(K_m + A)}}",
    MI = "\\Large{V = \\frac{V_{max} \\cdot A}{K_m \\left(1 + \\frac{I}{K_{ic}}\\right) + A \\left(1 + \\frac{I}{K_{iu}}\\right)}}",
    TC = "V = \\frac{V_{max} \\cdot A \\cdot B}{K_{mB} \\cdot A + K_{mA} \\cdot B + A \\cdot B + K_{sat} \\cdot K_{mB}}",
    HILL = "\\Large{V = V_{max} \\cdot \\frac{A^{n}}{K_m^{n} + A^{n}}}",
    PP = "\\Large{V = \\frac{V_{max} \\cdot A \\cdot B}{K_{mA} \\cdot B + K_{mB} \\cdot A + A \\cdot B}}",
    BK = "\\Large{F_B = \\frac{P}{P + K_D}}",
    QBK = "F_B = \\frac{(R + P + K_D) - \\sqrt{(R + P + K_D)^2 - 4 \\cdot R \\cdot P}}{2 \\cdot R}"
)

# Each model's variables in LaTeX
#' @export
MODEL_VARIABLES_DISPLAY <- list(
    MM = "{\\Large V}  -  \\text{Velocity of reaction} \\\\
          {\\Large V_{max}}  -  \\text{Maximum Velocity} \\\\
          {\\Large K_m}  -  \\text{Michaelis constant} \\\\
          {\\Large A}  -  \\text{Substrate A concentration}",
    
    MMSI = "{\\Large V}  -  \\text{Velocity of reaction} \\\\
            {\\Large V_{max}}  -  \\text{Maximum Velocity} \\\\
            {\\Large K_m}  -  \\text{Michaelis constant} \\\\
            {\\Large A}  -  \\text{Substrate A concentration} \\\\
            {\\Large K_{si}}  -  \\text{Substrate inhibition constant}",
    
    OGMM = "{\\Large V}  -  \\text{Velocity of reaction} \\\\
            {\\Large K_{cat}}  -  \\text{Turnover number} \\\\
            {\\Large E_0}  -  \\text{Initial enzyme concentration} \\\\
            {\\Large A}  -  \\text{Substrate A concentration} \\\\
            {\\Large K_m}  -  \\text{Michaelis constant}",
    
    ALTMM = "{\\Large V}  -  \\text{Velocity of reaction} \\\\
             {\\Large K_{cat}}  -  \\text{Turnover number} \\\\
             {\\Large K_A}  -  \\text{Association constant} \\\\
             {\\Large E_0}  -  \\text{Initial enzyme concentration} \\\\
             {\\Large A}  -  \\text{Substrate A concentration}",
    
    CI = "{\\Large V}  -  \\text{Velocity of reaction} \\\\
          {\\Large V_{max}}  -  \\text{Maximum Velocity} \\\\
          {\\Large K_m}  -  \\text{Michaelis constant} \\\\
          {\\Large A}  -  \\text{Substrate A concentration} \\\\
          {\\Large I}  -  \\text{Inhibitor concentration} \\\\
          {\\Large K_i}  -  \\text{Inhibition constant}",
    
    UCI = "{\\Large V}  -  \\text{Velocity of reaction} \\\\
           {\\Large V_{max}}  -  \\text{Maximum Velocity} \\\\
           {\\Large K_m}  -  \\text{Michaelis constant} \\\\
           {\\Large A}  -  \\text{Substrate A concentration} \\\\
           {\\Large I}  -  \\text{Inhibitor concentration} \\\\
           {\\Large K_i}  -  \\text{Inhibition constant}",
    
    NCI = "{\\Large V}  -  \\text{Velocity of reaction} \\\\
           {\\Large V_{max}}  -  \\text{Maximum Velocity} \\\\
           {\\Large K_m}  -  \\text{Michaelis constant} \\\\
           {\\Large A}  -  \\text{Substrate A concentration} \\\\
           {\\Large I}  -  \\text{Inhibitor concentration} \\\\
           {\\Large K_i}  -  \\text{Inhibition constant}",
    
    MI = "{\\Large V}  -  \\text{Velocity of reaction} \\\\
          {\\Large V_{max}}  -  \\text{Maximum Velocity} \\\\
          {\\Large K_m}  -  \\text{Michaelis constant} \\\\
          {\\Large A}  -  \\text{Substrate A concentration} \\\\
          {\\Large I}  -  \\text{Inhibitor concentration} \\\\
          {\\Large K_{ic}}  -  \\text{Competitive inhibition constant} \\\\
          {\\Large K_{iu}}  -  \\text{Uncompetitive inhibition constant}",
    
    TC = "{\\Large V}  -  \\text{Velocity of reaction} \\\\
          {\\Large V_{max}}  -  \\text{Maximum Velocity} \\\\
          {\\Large A}  -  \\text{Substrate A concentration} \\\\
          {\\Large B}  -  \\text{Substrate B concentration} \\\\
          {\\Large K_{mA}}  -  \\text{Michaelis constant for A} \\\\
          {\\Large K_{mB}}  -  \\text{Michaelis constant for B} \\\\
          {\\Large K_{sat}}  -  \\text{Saturation constant}",
    
    HILL = "{\\Large V}  -  \\text{Velocity of reaction} \\\\
            {\\Large V_{max}}  -  \\text{Maximum Velocity} \\\\
            {\\Large A}  -  \\text{Substrate A concentration} \\\\
            {\\Large K_m}  -  \\text{Michaelis constant} \\\\
            {\\Large n}  -  \\text{Hill coefficient}",
    
    PP = "{\\Large V}  -  \\text{Velocity of reaction} \\\\
          {\\Large V_{max}}  -  \\text{Maximum Velocity} \\\\
          {\\Large A}  -  \\text{Substrate A concentration} \\\\
          {\\Large B}  -  \\text{Substrate B concentration} \\\\
          {\\Large K_{mA}}  -  \\text{Michaelis constant for A} \\\\
          {\\Large K_{mB}}  -  \\text{Michaelis constant for B}",
    
    BK = "{\\Large F_B}  -  \\text{Fraction bound} \\\\
           {\\Large P}  -  \\text{Binding partner in excess concentration} \\\\
           {\\Large K_D}  -  \\text{Dissociation constant}",
    
    QBK = "{\\Large F_B}  -  \\text{Fraction bound} \\\\
           {\\Large R}  -  \\text{Trace limiting partner concentration} \\\\
           {\\Large P}  -  \\text{Binding partner in excess concentration} \\\\
           {\\Large K_D}  -  \\text{Dissociation constant}"
)

# Each plot transformation's function
#' @export
PLOT_TRANSFORMATIONS <- list(
    standard = function(data.df, x.name = NULL, y.name = NULL) {
        # No transformation
        return (data.df)
    },
    log10 = function(data.df, x.name = "A", y.name = "V") {
        # Log transform the x.axis
        data.df[[x.name]] <- log10(data.df[[x.name]])
        return (data.df)
    },
    lineweaver = function(data.df, x.name = "A", y.name = "V") {
        # Get reciprocal of the x-axis
        data.df[[x.name]] <- 1 / data.df[[x.name]]
        # Get reciprocal of the y-axis
        data.df[[y.name]] <- 1 / data.df[[y.name]]
        return(data.df)
    },
    hanes = function(data.df, x.name = "A", y.name = "V") {
        # Get x/y for the y-axis
        data.df[[y.name]] <- data.df[[x.name]] / data.df[[y.name]]
        return(data.df)
    },
    eadie = function(data.df, x.name = "A", y.name = "V") {
        # Get y/x for the x-axis
        data.df[[x.name]] <- data.df[[y.name]] / data.df[[x.name]]
        return(data.df)
    },
    direct = function(data.df, x.name = NULL, y.name = NULL) {
        # No transformation (transformation done elsewhere)
        return (data.df)
    }
)

# All plot transformation options
#' @export
PLOT_TRANSFORMATION_OPTIONS <- c(
    "Standard Plot (v against a)" = "standard",
    "Log 10 Plot (v against log10 a)" = "log10",
    "Lineweaver-Burk Plot (1/v against 1/a)" = "lineweaver",
    "Hanes Plot (a/v against a)" = "hanes",
    "Eadie-Hofstee Plot (v against v/a)" = "eadie",
    "Direct Linear Plot (Vmax against Km)" = "direct"
)

# Each plot transformation's plot title
#' @export
PLOT_TRANSFORMATION_TITLES <- c(
    standard = "",
    log10 = "Log 10 Plot",
    lineweaver = "Lineweaver-Burk Plot",
    hanes = "Hanes Plot",
    eadie = "Eadie-Hofstee Plot",
    direct = "Direct Linear Plot"
)

# Each plot transformation's x-axis title
#' @export
PLOT_TRANSFORMATION_X_AXIS_TITLES <- c(
    standard = "",
    log10 = "Log 10 of",
    lineweaver = "Reciprocal of",
    hanes = "",
    eadie = "Y-AXIS /",
    direct = "Km"
)

# Each plot transformation's y-axis title
#' @export
PLOT_TRANSFORMATION_Y_AXIS_TITLES <- c(
    standard = "",
    log10 = "",
    lineweaver = "Reciprocal of",
    hanes = "X-AXIS /",
    eadie = "",
    direct = "Vmax"
)

# Each model's blocked transformations
#' @export
BLOCKED_TRANSFORMATIONS <- list(
    MM = c(),
    MMSI = c("lineweaver", "hanes", "eadie", "direct"),
    OGMM = c("direct"),
    ALTMM = c("direct"),
    CI = c("direct"),
    UCI = c("direct"),
    NCI = c("direct"),
    MI = c("direct"),
    TC = c("direct"),
    HILL = c("lineweaver", "hanes", "eadie", "direct"),
    PP = c("direct"),
    BK = c("lineweaver", "hanes", "eadie", "direct"),
    QBK = c("lineweaver", "hanes", "eadie", "direct")
)

# All fitting method options
#' @export
FITTING_METHODS_OPTIONS <- c(
    "Nonlinear Least Squares" = "nls",
    "Recursive Solve" = "recursive",
    "Sum of Squares Calculation" = "ss.calc",
    "Nonparametric" = "nonparametric"
)

# Each fitting method's valid models
#' @export
FITTING_METHODS <- list(
    nls = c("MM", "MMSI", "OGMM", "ALTMM", "CI", "UCI", "NCI", "MI", "TC", "HILL", "PP", "BK", "QBK"),
    recursive = c("MM"),
    ss.calc = c("MM"),
    nonparametric = c("MM")
)

# Each model's blocked fitting methods
#' @export
BLOCKED_FITTING_METHODS <- list(
    MM = c(),
    MMSI = c("recursive", "ss.calc", "nonparametric"),
    OGMM = c("recursive", "ss.calc", "nonparametric"),
    ALTMM = c("recursive", "ss.calc", "nonparametric"),
    CI = c("recursive", "ss.calc", "nonparametric"),
    UCI = c("recursive", "ss.calc", "nonparametric"),
    NCI = c("recursive", "ss.calc", "nonparametric"),
    MI = c("recursive", "ss.calc", "nonparametric"),
    TC = c("recursive", "ss.calc", "nonparametric"),
    HILL = c("recursive", "ss.calc", "nonparametric"),
    PP = c("recursive", "ss.calc", "nonparametric"),
    BK = c("recursive", "ss.calc", "nonparametric"),
    QBK = c("recursive", "ss.calc", "nonparametric")
)

# Each model's bounding variable names
CONFIDENCE_INTERVAL_BOUNDING_VARIABLES <- list(
    MM = c("A.lb", "A.ub", "V.lb", "V.ub"),
    MMSI = c("A.lb", "A.ub", "V.lb", "V.ub"),
    OGMM = c("A.lb", "A.ub", "V.lb", "V.ub"),
    ALTMM = c("A.lb", "A.ub", "V.lb", "V.ub"),
    CI = c("A.lb", "A.ub", "V.lb", "V.ub", "I.lb", "I.ub"),
    UCI = c("A.lb", "A.ub", "V.lb", "V.ub", "I.lb", "I.ub"),
    NCI = c("A.lb", "A.ub", "V.lb", "V.ub", "I.lb", "I.ub"),
    MI = c("A.lb", "A.ub", "V.lb", "V.ub", "I.lb", "I.ub"),
    TC = c("A.lb", "A.ub", "V.lb", "V.ub", "B.lb", "B.ub"),
    HILL = c("A.lb", "A.ub", "V.lb", "V.ub"),
    PP = c("A.lb", "A.ub", "V.lb", "V.ub", "B.lb", "B.ub"),
    BK = c("P.lb", "P.ub", "FB.lb", "FB.ub"),
    QBK = c("P.lb", "P.ub", "FB.lb", "FB.ub", "R.lb", "R.ub")
)

# Each model's bounding parameter names
CONFIDENCE_INTERVAL_BOUNDING_PARAMS <- list(
    MM = c("Km.lb", "Km.ub", "Vmax.lb", "Vmax.ub"),
    MMSI = c("Km.lb", "Km.ub", "Vmax.lb", "Vmax.ub", "Ksi.lb", "Ksi.ub"),
    OGMM = c("Km.lb", "Km.ub", "Kcat.lb", "Kcat.ub", "E0.lb", "E0.ub"),
    ALTMM = c("KA.lb", "KA.ub", "Kcat.lb", "Kcat.ub", "E0.lb", "E0.ub"),
    CI = c("Km.lb", "Km.ub", "Vmax.lb", "Vmax.ub", "Ki.lb", "Ki.ub"),
    UCI = c("Km.lb", "Km.ub", "Vmax.lb", "Vmax.ub", "Ki.lb", "Ki.ub"),
    NCI = c("Km.lb", "Km.ub", "Vmax.lb", "Vmax.ub", "Ki.lb", "Ki.ub"),
    MI = c("Km.lb", "Km.ub", "Vmax.lb", "Vmax.ub", "Kic.lb", "Kic.ub", "Kiu.lb", "Kiu.ub"),
    TC = c("KmA.lb", "KmA.ub", "Vmax.lb", "Vmax.ub", "KmB.lb", "KmB.ub", "Ksat.lb", "Ksat.ub"),
    HILL = c("Km.lb", "Km.ub", "Vmax.lb", "Vmax.ub", "n.lb", "n.ub"),
    PP = c("KmA.lb", "KmA.ub", "Vmax.lb", "Vmax.ub", "KmB.lb", "KmB.ub"),
    BK = c("KD.lb", "KD.ub"),
    QBK = c("KD.lb", "KD.ub")
)

# Each parameter's bound that gives the lower bound for its function
LOWER_BOUND_PARAMS <- list(
    Km = "Km.ub", 
    KmA = "KmA.ub",
    KmB = "KmB.ub",
    Ksi = "Ksi.lb",
    Ki = "Ki.lb",
    Kic = "Kic.lb",
    Kiu = "Kiu.lb",
    Ksat = "Ksat.ub",
    Vmax = "Vmax.lb",
    n = c("n.lb", "n.ub"),
    KD = "KD.ub",
    Kcat = "Kcat.lb",
    E0 = "E0.lb",
    KA = "KA.lb"
)

# Each parameter's bound that gives the upper bound for its function
UPPER_BOUND_PARAMS <- list(
    Km = "Km.lb", 
    KmA = "KmA.lb",
    KmB = "KmB.lb",
    Ksi = "Ksi.ub",
    Ki = "Ki.ub",
    Kic = "Kic.ub",
    Kiu = "Kiu.ub",
    Ksat = "Ksat.lb",
    Vmax = "Vmax.ub",
    n = c("n.lb", "n.ub"),
    KD = "KD.lb",
    Kcat = "Kcat.ub",
    E0 = "E0.ub",
    KA = "KA.ub"
)

# All valid plot colour palettes
#' @export
PLOT_COLOUR_PALETTES <- c("Set1", "Set2", "Set3", "Accent", "Dark2", 
                           "Pastel2", "Pastel1", "Blues", "Greys", "Oranges", 
                           "Purples", "Greens", "Reds", "OrRd", "PuBu", "BuPu", 
                           "YlOrBr", "RdPu", "GnBu", "YlOrRd", "PuRd", 
                           "RdYlGn", "RdGy", "RdBu")

# Each model's function to simulate perfect data
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
        n <- params$n
        # Create a data frame with all combinations of A.range
        grid <- expand.grid(A = A.range)
        # Calculate V for each combination
        grid$V <- Vmax * (grid$A^n) / (Km^n + grid$A^n)
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
    BK = function(params, P.range, z.range) {
        # Extract parameters
        KD <- params$KD
        # Create a data frame with all combinations of P.range
        grid <- expand.grid(P = P.range)
        # Calculate FB for each combination
        grid$FB <- grid$P / (grid$P + KD)
        # Return data frame
        return(grid)
    },
    QBK = function(params, P.range, R.range) {
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
