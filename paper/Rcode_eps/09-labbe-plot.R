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

pkgroot = rprojroot::find_package_root_file()


library(SubgrPlots) # Loads this package. Install it first
library(survival)
library(ggplot2)
library(dplyr)

# # Load the data to be used
data(prca)
dat <- prca
dat %>%
  mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
         hx = factor(ifelse(hx == 0 , "No", "Yes")))-> dat

###############################################################################-
## 8. Labbe Plot -----------------------------------------------------------
filename = "09-labbe-plot"
width    = 6
height   = 6
res = 600
setEPS()
postscript(paste0(rprojroot::find_package_root_file(),
                  "/paper/figures_eps/", filename, ".eps"),
           width = width, height = height)
# postscript("paper/figures_eps/09-labbe-plot.eps", width = 6, height = 6)
# pdf("paper/figures/09-labbe-plot.pdf", width = 6, height = 6)

lab.xy = list("Control Group Estimate", "Treatment Group Estimate")
plot_labbe(dat = dat,
           covari.sel = c(4,5,6,7),
           trt.sel = 3,
           resp.sel = c(1,2),
           outcome.type = "survival",
           effect = "RMST",
           lab.xy = lab.xy,
           size.shape = 0.2,
           adj.ann.subgrp = 1/30,
           time=50, show.ci = FALSE)
dev.off()

