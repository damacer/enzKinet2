# enzKinet2 2.1.0

* `fit.method = "recursive"` now repeats its weighted calculation until the
  estimates settle, as described by Cornish-Bowden (equations 15.10-15.12).
  It used to stop after the first round, so its estimates were only approximate.
  It now gives the least-squares estimates, the same as `"nls"`.
* New documentation website at <https://eka.nz/docs>, with guides, worked
  examples and a user guide for the EKA2 web app.
* Every help page is rewritten and has examples.
* Corrected some parameter descriptions in `MODEL_VARIABLES_DISPLAY` (shown in
  EKA2). `KA` in Alternative Michaelis-Menten is the specificity constant
  (kcat/Km), not an association constant. `Km` in Hill is the half-saturation
  constant K0.5. `KD` in Hill Binding Kinetics is a half-saturation constant.
  `Ksat` in Ternary Complex is the dissociation constant of EA (KiA). `E0` is
  the total enzyme concentration. `R` in Quadratic Binding Kinetics is the
  limiting partner's concentration, which needn't be at trace levels.
  Parameter names are unchanged.
* The exported constants are documented in `?model_registry` and
  `?interface_constants`, which replace `?package_constants`.
* Axis units on Lineweaver-Burk and Hanes plots are bracketed where needed,
  e.g. 1/(µM/min) rather than 1/µM/min.
* `citation("enzKinet2")` gives citation information.
* RColorBrewer, stats and utils are declared in Imports.

# enzKinet2 2.0.0 to 2.0.7

2.0.0 (December 2024) was a complete rewrite of the package for EKA2. The 2.0.x
releases that followed added the Hill Binding Kinetics model, the `y.range`
and `show.grid` plot options, a choice of confidence interval method
(`conf.method`), and many smaller fixes.
