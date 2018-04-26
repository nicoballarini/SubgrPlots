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

# dat
var.sel = 8
main.title = paste("STEPP for treatment effect size of overlapping subgroups \n defined by", names(dat)[var.sel]);
lab.y.title = paste("Treatment effect diffence");
setup.ss = c(35, 40)
sub.title = paste0("(Subgroup sample sizes are set to ", setup.ss[2], "; overlap sizes are set to ", setup.ss[1], ")" )
covari.sel = 8
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
setup.ss = c(35,40)
alpha = 0.05
font.size = c(0.9, 1, 0.8, 0.75)
title = main.title
lab.y = lab.y.title
subtitle = sub.title

###############################################################################-
## 9. stepp Plot -----------------------------------------------------------
pdf("paper/figures/09-stepp-plot.pdf", width = 7, height = 7)
var.sel = 8
main.title = paste("STEPP for treatment effect size of overlapping subgroups \n defined by", names(dat)[var.sel]);
lab.y.title = paste("Treatment effect diffence");
sub.title = paste0("(Subgroup sample sizes are set to ", setup.ss[2], "; overlap sizes are set to ", setup.ss[1], ")" )
setup.ss = c(35, 40);
plot_stepp(dat,
          covari.sel = 8,
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          setup.ss = c(35,40),
          alpha = 0.05,
          font.size = c(0.9, 1, 0.8, 0.75),
          title = NULL,
          lab.y = lab.y.title,
          subtitle = sub.title)
dev.off()

