###############################################################################-
##
## This program creates the figures for the manuscript using the
## prca data that is included in this package
##
##
## Instead of using rm(list = ls()), make sure you start with a fresh R
## by restarting R -> Control+Shift+F10
# cat("\014") # Cleans the console
## Load needed libraries
## If SubgrPlots package is not installed, then open project and use following lines or
## in the build window, click Install and Restart
# devtools::build()
# devtools::install()
library(SubgrPlots)
data(prca)
pdf("paper/figures/03-B-contour-plot-localreg.pdf", width = 6, height = 5)
plot_contour_localreg(prca,
               covari.sel = c(8,9),
               trt.sel = 3,
               resp.sel = c(1,2),
               strip = "Treatment effect size (log hazard ratio)",
               outcome.type = "survival")
dev.off()
