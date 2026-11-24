
# enzKinet2

<!-- badges: start -->
[![R-CMD-check](https://github.com/damacer/enzKinet2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/damacer/enzKinet2/actions/workflows/R-CMD-check.yaml)
[![Documentation](https://img.shields.io/badge/docs-eka.nz%2Fdocs-608AC8)](https://eka.nz/docs)
[![License: GPL v3](https://img.shields.io/badge/license-GPL--3-blue)](https://github.com/damacer/enzKinet2/blob/master/LICENSE.md)
<!-- badges: end -->

enzKinet2 is an R package for enzyme kinetics. It simulates, fits and plots 14 models of enzyme and binding kinetics, and it powers [EKA2](https://eka.nz), the free enzyme kinetics web app.

**Documentation, guides and worked examples: <https://eka.nz/docs>**

## Installation

enzKinet2 isn't on CRAN. Install it from GitHub:

```r
# install.packages("remotes")
remotes::install_github("damacer/enzKinet2")
```

## Example

```r
library(enzKinet2)

# Simulate Michaelis-Menten data: 8 concentrations in triplicate, with 5% noise
set.seed(1)
data <- simulate_data("MM", params = list(Km = 0.5, Vmax = 2),
                      x.min = 0.1, x.max = 5, n.samples = 8, n.replicates = 3)

# Fit the model, with 95% confidence intervals
fit <- fit_model("MM", data, start.params = list(Km = 1, Vmax = 1),
                 get.conf.int = TRUE)
fit$fitted.params

# Plot the data, the fitted curve and its parameter uncertainty envelope
curve <- make_curve("MM", fit$fitted.params, x.min = 0, x.max = 5, conf.int = TRUE)
make_plot("MM", data.df = data, curve.df = curve, conf.int = TRUE)
```

[Get started](https://eka.nz/docs/articles/enzKinet2.html) walks through this in more detail.

## Models

| Group | Models |
|---|---|
| Michaelis-Menten | Michaelis-Menten (`MM`), with substrate inhibition (`MMSI`), written with kcat and E0 (`OGMM`), written with kcat and the specificity constant (`ALTMM`), Hill (`HILL`) |
| Inhibition | Competitive (`CI`), uncompetitive (`UCI`), non-competitive (`NCI`), mixed (`MI`) |
| Two substrates | Ternary complex (`TC`), ping-pong (`PP`) |
| Binding | Binding (`BK`), Hill binding (`BKH`), quadratic binding with ligand depletion (`QBK`) |

Every model can be fitted by nonlinear least squares, with profile likelihood or asymptotic confidence intervals. Plots can be drawn as standard, log10, Lineweaver-Burk, Hanes, Eadie-Hofstee or direct linear plots. The [model gallery](https://eka.nz/docs/articles/concepts/models.html) has each model's equation.

## EKA2

<p align="center">
    <img src="man/figures/logo.png" alt="EKA Logo" width="200">
</p>

[EKA2](https://eka.nz) is a free web app for enzyme kinetics, built on enzKinet2. It does the same simulating, fitting and plotting without any code, and is designed for teaching as well as research. Its [user guide](https://eka.nz/docs/articles/eka2/user-guide.html) is part of the documentation.

## Citation

If you use enzKinet2 or EKA2, please cite:

Mak DA, Dunn S, Coombes D, Carere CR, Allison JR, Nock V, Hudson AO, Dobson RCJ (2024). Enzyme Kinetics Analysis: An online tool for analyzing enzyme initial rate data and teaching enzyme kinetics. *Biochemistry and Molecular Biology Education* 52(3): 348–358. <https://doi.org/10.1002/bmb.21823>

`citation("enzKinet2")` gives this in BibTeX and other formats.

## Authors

enzKinet2 is written by Haig Bishop, with Daniel Mak and Ren Dobson. Questions and feedback are welcome at <haigvbishop@gmail.com>, and bug reports on [GitHub](https://github.com/damacer/enzKinet2/issues).

Copyright © 2024–2026 Haig Bishop. enzKinet2 is free software under the [GNU General Public License, version 3 or later](LICENSE.md).
