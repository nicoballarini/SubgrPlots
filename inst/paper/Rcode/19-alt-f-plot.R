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

library(SubgrPlots) # Loads this package. Install it first
# # Load the data to be used
data(prca)
dat <- prca
## 1. Level plot ---------------------------------------------------------------
pdf("paper/figures/19-alt-f-plot.pdf", width = 5, height = 4)
plot_dissimilarity_alternative_new(dat = dat,
                               covari.sel = c(4,5,6),
                               mode = 2,
                               range.ds = c(0,1),
                               font.size = c(1, 1, 0.7),
                               title = NULL,
                               lab.y = "Similarity distance")
dev.off()
