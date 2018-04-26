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

dat
covari.sel = c(6,5,4,7)
mode = 1
para = c(0, 0.6, 1)
font.size = c(1.2, 1.2)
title = NULL

###############################################################################-
## 1. Level plot ---------------------------------------------------------------
pdf("paper/figures/13-alt-b-plot.pdf", width = 5, height = 5)
main.title = "Relative overlapping proportions of pairwise subgroups"
plot_overlap_alternative(dat = dat,
                         covari.sel = c(6,5,4,7),
                         mode = 1,
                         para = c(0, 0.6, 1),
                         font.size = c(1.2, 1.2),
                         title = NULL)
dev.off()


plot_overlap_alternative2(dat = dat,
                         covari.sel = c(6,5,4,7),
                         mode = 1,
                         para = c(0, 0.6, 1),
                         font.size = c(1.2, 1.2),
                         title = NULL)
