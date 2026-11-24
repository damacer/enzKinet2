# EKA Documentation

This is the documentation for **EKA2**, the free enzyme kinetics web app at [eka.nz](https://eka.nz), and for **enzKinet2**, the R package behind it. Both simulate, fit and plot the same 14 models of enzyme and binding kinetics, from Michaelis-Menten to two-substrate and binding models.

<div class="eka-cards">
<div class="eka-card">
<h2>R Package (enzKinet2)</h2>
<p>Simulate, fit and plot kinetics models in R, with full control and reproducible scripts.</p>
<p><a href="articles/enzKinet2.html">Get started</a> · <a href="articles/r/example-michaelis-menten.html">Worked examples</a> · <a href="reference/index.html">Function reference</a></p>
</div>
<div class="eka-card">
<h2>Web App (EKA2)</h2>
<p>Do the same in your browser, with no code: explore models with live sliders, simulate or upload data, and fit.</p>
<p><a href="articles/eka2/quick-start.html">Quick start</a> · <a href="articles/eka2/user-guide.html">User guide</a> · <a class="eka-ext" href="https://eka.nz" target="_blank" rel="noopener">Open EKA2</a></p>
</div>
<div class="eka-card">
<h2>Enzyme Kinetics Concepts</h2>
<p>The models, fitting methods and plots behind both tools, explained.</p>
<p><a href="articles/concepts/models.html">Models</a> · <a href="articles/concepts/fitting.html">Fitting</a> · <a href="articles/concepts/plots.html">Plot types</a></p>
</div>
</div>

## Install enzKinet2

enzKinet2 isn't on CRAN. Install it from [GitHub](https://github.com/damacer/enzKinet2):

```r
# install.packages("remotes")
remotes::install_github("damacer/enzKinet2")
```

## Cite

If you use EKA2 or enzKinet2 in your work, please cite the EKA paper:

Mak DA, Dunn S, Coombes D, Carere CR, Allison JR, Nock V, Hudson AO, Dobson RCJ (2024). Enzyme Kinetics Analysis: An online tool for analyzing enzyme initial rate data and teaching enzyme kinetics. *Biochemistry and Molecular Biology Education* 52(3): 348–358. <https://doi.org/10.1002/bmb.21823>

## About

EKA began as some R code written by Ren Dobson. Daniel Mak developed it into a cohesive tool and put it online as EKA and enzKinet2, and Haig Bishop rewrote and expanded it as EKA2. Questions and feedback are welcome at <haigvbishop@gmail.com>, and bug reports on [GitHub](https://github.com/damacer/enzKinet2/issues).
