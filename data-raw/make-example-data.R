# Makes the example data files used in the documentation (run from the package root):
#   inst/extdata/          read by the R articles with system.file()
#   pkgdown/assets/data/   copied to eka.nz/docs/data/ for EKA2 users to download
# The data are simulated, so the true parameter values are known (see below).
pkgload::load_all(".", quiet = TRUE)
set.seed(2026)

# Michaelis-Menten, true Km = 0.8 mM, Vmax = 12 µM/min. 7 concentrations in triplicate, 5% noise.
mm <- simulate_data("MM", list(Km = 0.8, Vmax = 12), x.min = 0.1, x.max = 8,
                    space = "custom", x.range = c(0.1, 0.25, 0.5, 1, 2, 4, 8),
                    n.replicates = 3, noise.level = 0.05)
mm <- data.frame(substrate_mM = mm$A, rate_uM_min = round(mm$V, 2))
mm <- mm[order(mm$substrate_mM), ]

# Mixed inhibition, true Km = 0.8 mM, Vmax = 12 µM/min, Kic = 20 µM, Kiu = 60 µM.
# 6 substrate concentrations at 4 inhibitor concentrations, in duplicate, 4% noise.
inh <- simulate_data("MI", list(Km = 0.8, Vmax = 12, Kic = 20, Kiu = 60), x.min = 0.2, x.max = 8,
                     space = "custom", x.range = c(0.2, 0.5, 1, 2, 4, 8), z.values = c(0, 10, 25, 50),
                     n.replicates = 2, noise.level = 0.04)
inh <- data.frame(substrate_mM = inh$A, inhibitor_uM = inh$I, rate_uM_min = round(inh$V, 2))
inh <- inh[order(inh$inhibitor_uM, inh$substrate_mM), ]

for (dir in c("inst/extdata", "pkgdown/assets/data")) {
    write.csv(mm, file.path(dir, "michaelis-menten.csv"), row.names = FALSE)
    write.csv(inh, file.path(dir, "inhibition.csv"), row.names = FALSE)
}
