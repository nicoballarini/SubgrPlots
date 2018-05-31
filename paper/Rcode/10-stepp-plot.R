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
## 9. stepp Plot -----------------------------------------------------------
pdf("paper/figures/10-stepp-plot.pdf", width = 7, height = 7)
lab.y.title = paste("Treatment effect diffence");
setup.ss = c(30,40)
sub.title = paste0("(Subgroup sample sizes are set to ", setup.ss[2], "; overlap sizes are set to ", setup.ss[1], ")" )
plot_stepp(dat,
          covari.sel = 8,
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          setup.ss = c(30,40),
          alpha = 0.05,
          font.size = c(0.9, 1, 1, 1),
          title = NULL,
          lab.y = lab.y.title,
          subtitle = sub.title)
dev.off()
