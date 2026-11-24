# Helpers used only when roxygen2 builds the help pages (via @eval).

# The table of models at the top of the model_registry help page, built from the registry
# itself so that it always matches the code. roxygen2 adds it to the page's description.
rd_model_table <- function() {
    code <- function(x) paste0("`", x, "`")
    rows <- vapply(VALID_MODELS, function(m) {
        vars <- MODEL_VARIABLES[[m]]
        extra <- if (length(vars) > 2) code(vars[3]) else "-"
        paste0("| ", code(m), " | ", PLOT_TITLES[[m]], " | ", code(vars[1]), " | ", code(vars[2]),
               " | ", extra, " | ", paste(code(MODEL_PARAMETERS[[m]]), collapse = ", "), " |")
    }, character(1))
    c("@description",
      "Each model's variables are the primary independent variable (x), the dependent variable (y) and, for some models, an extra independent variable.",
      "",
      "| **Code** | **Model** | **x** | **y** | **Extra** | **Parameters** |",
      "|------|-------|---|---|-------|------------|",
      rows)
}
