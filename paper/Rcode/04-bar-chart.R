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

levels(dat$age_group) = c("Young","Middle-aged","Old")
levels(dat$weight_group)  = c("Low","Mid","High")
dat
covari.sel = c(14,15)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
main.title = paste("Treatment effect sizes across subgroups", sep = "");
lab.y.title = paste("Treatment effect size (log hazard ratio)");
title = main.title
lab.y = lab.y.title

font.size = c(14, 12, 14, 0.9)

names(dat)[covari.sel] = c("age", "weight")

###############################################################################-
## 4. Bar chart -----------------------------------------------------------
pdf("paper/figures/04-bar-chart.pdf", width = 7, height = 7)
main.title = paste("Treatment effect sizes across subgroups", sep = "");
lab.y.title = paste("Treatment effect size (RMST difference)");
plot_barchart(dat,
       covari.sel = c(14,15),
       trt.sel = 3,
       resp.sel = c(1, 2),
       outcome.type = "survival",
       font.size = c(14, 12, 14, 0.75),
       title = NULL,
       lab.y = lab.y.title)
dev.off()
