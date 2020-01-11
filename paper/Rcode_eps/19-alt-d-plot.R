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
library(survival)
library(ggplot2)
library(dplyr)

# # Load the data to be used
data(prca)
dat <- prca

###############################################################################-
## 1. Matrix Overlap plot ------------------------------------------------------
pdf("paper/figures/19-alt-d-plot.pdf", width = 4.1, height = 4)
plot_matrix_overlap(dat,
                    covari.sel = c(6,5,4,7),
                    mode = 1,
                    font.size = c(1.5, 1.25, 0.8),
                    title = NULL)
dev.off()

