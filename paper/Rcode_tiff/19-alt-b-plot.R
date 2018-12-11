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

pkgroot = rprojroot::find_package_root_file()

library(SubgrPlots) # Loads this package. Install it first
# # Load the data to be used
data(prca)
dat <- prca
###############################################################################-
## 1. Overlap alternative plot -------------------------------------------------
filename = "19-alt-b-plot"
width    = 4
height   = 4
res = 600
tiff(paste0(rprojroot::find_package_root_file(),"/paper/figures_tiff/", filename, ".tiff"),
     width = width, height = height, units = "in", res = res)
# pdf("paper/figures/19-alt-b-plot.pdf", width = 4, height = 4)
plot_overlap_alternative(dat = dat,
                         covari.sel = c(6,5,4,7),
                         mode = 1,
                         para = c(0, 0.6, 1),
                         font.size = c(1.2, 1.2, 0.8),
                         title = NULL)
dev.off()

