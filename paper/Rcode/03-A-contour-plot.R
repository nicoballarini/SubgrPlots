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
library(dplyr)
# Load the data to be used
data(prca)
dat <- prca
setup.ss =  c(10,60,15,30)
dat %>%
  rename(Weight = weight,
         Age = age) -> dat

pdf("paper/figures/03-A-contour-plot.pdf", width = 5, height = 4)
plot_contour(dat,
             covari.sel = c(8,9),
             trt.sel = 3,
             resp.sel = c(1,2),
             outcome.type = "survival",
             setup.ss =  setup.ss,
             n.grid = c(100,100),
             brk.es = seq(-4.5,4.5,length.out = 101),
             n.brk.axis =  7,
             para.plot = c(0.5, 2, 6),
             font.size = c(1, 1, 1, 1, 1),
             title = NULL,
             strip = paste("Treatment effect size (log hazard ratio)"),
             show.overall = T,show.points = T,
             filled = T, palette = "hcl", col.power = 0.75)
dev.off()
