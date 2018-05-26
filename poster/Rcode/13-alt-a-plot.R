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
vars = data.frame(variable = names(dat), index = 1:length(names(dat)))
covari.sel = c(4, 5, 6, 7)
para = c(0.05, 0.15, 1)
font.size = c(1.5, 1.5)
main.title = "Relative overlapping proportions of pairwise subgroups"
title = main.title

###############################################################################-
## 1. Level plot ---------------------------------------------------------------
pdf("paper/figures/13-alt-a-plot.pdf", width = 5, height = 5)
main.title = "Relative overlapping proportions of pairwise subgroups"
plot_overlap(dat = dat,
             covari.sel = c(6,5,4,7),
             para = c(0.1, 0.5, 1),
             font.size = c(1.2, 1.2),
             title = NULL)
dev.off()

pdf("paper/figures/13-alt-a-plot-matrix.pdf", width = 7, height = 5)
main.title = "Relative overlapping proportions of pairwise subgroups"
plot_overlap2(dat = dat,
             covari.sel = c(6,5,4,7),
             para = c(0.05, 0.75, 1),
             font.size = c(1.2, 1.2),
             title = NULL)
dev.off()
