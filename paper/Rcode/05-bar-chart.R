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
levels(dat$age_group) = c("Young","Middle-aged","Old")
levels(dat$weight_group)  = c("Low","Mid","High")
names(dat)[c(14,15)] = c("Age", "Weight")

###############################################################################-
## 4. Bar chart -----------------------------------------------------------
pdf("paper/figures/05-bar-chart.pdf", width = 5, height = 5)
plot_barchart(dat,
              covari.sel = c(14,15),
              trt.sel = 3,
              resp.sel = c(1, 2),
              outcome.type = "survival",
              font.size = c(12, 12, 12, 1), time = 50,
              lab.y = "Treatment effect size (RMST difference)")
dev.off()
