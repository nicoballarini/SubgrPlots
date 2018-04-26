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
covari.sel = c(14,15)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"

vars
names(dat)[covari.sel] = c("age", "weight")

###############################################################################-
## 1. Level plot ---------------------------------------------------------------
pdf("paper/figures/13-alt-f-plot.pdf", width = 5, height = 5)
main.title = "Similarity measure of pairwise subgroups";
label.y = paste("Similarity distance ");
plot_dissimilarity_alternative(dat = dat,
                              covari.sel = c(4,5,6),
                              mode = 2,
                              range.ds = c(0,1),
                              font.size = c(1, 1, 0.7),
                              title = NULL,
                              lab.y = label.y)
dev.off()

## 1. Level plot ---------------------------------------------------------------
pdf("paper/figures/13-alt-f-plot.pdf", width = 5, height = 5)
main.title = "Similarity measure of pairwise subgroups";
label.y = paste("Similarity distance ");
plot_dissimilarity_alternative_new(dat = dat,
                               covari.sel = c(4,5,6),
                               mode = 2,
                               range.ds = c(0,1),
                               font.size = c(1, 1, 0.7),
                               title = NULL,
                               lab.y = label.y)
dev.off()
