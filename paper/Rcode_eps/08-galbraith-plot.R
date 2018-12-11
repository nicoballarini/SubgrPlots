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
library(dplyr)

# # Load the data to be used
data(prca)
dat <- prca
dat %>%
  mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
         hx = factor(ifelse(hx == 0 , "No", "Yes")))-> dat


filename = "08-galbraith-plot-naive"
width    = 5
height   = 5
res = 600
setEPS()
postscript(paste0(rprojroot::find_package_root_file(),
                  "/paper/figures_eps/", filename, ".eps"),
           width = width, height = height)
# postscript("paper/figures_eps/08-galbraith-plot-naive.eps", width = 5, height = 5)
# pdf("paper/figures/08-galbraith-plot-naive.pdf", width = 5, height = 5)
label.xy = list(expression(1/hat(sigma)[hat(delta)[i]]),
                         expression((hat(delta)[i]-hat(delta)[F])/hat(sigma)[hat(delta)[i]]))
plot_radial(dat,
            covari.sel = c(4, 5, 6, 7),
            trt.sel = 3,
            resp.sel = c(1, 2),
            outcome.type = "survival",
            range.v = c(-7, 6),
            adj.ann.subgrp = 4,
            font.size = c(1, 1, 0.75, 0.8, 0.85),
            lab.xy = label.xy)
dev.off()


filename = "08-galbraith-plot"
width    = 5
height   = 5
setEPS()
postscript(paste0(rprojroot::find_package_root_file(),
                  "/paper/figures_eps/", filename, ".eps"),
           width = width, height = height)
# pdf("paper/figures/08-galbraith-plot.pdf", width = 5, height = 5)
label.xy = list(expression(1/sqrt(hat(Var)(hat(delta)[i]-hat(delta)[F]))),
                         expression((hat(delta)[i]-hat(delta)[F])/sqrt(hat(Var)(hat(delta)[i]-hat(delta)[F]))))
plot_radial2(dat,
            covari.sel = c(4, 5, 6, 7),
            trt.sel = 3,
            resp.sel = c(1, 2),
            outcome.type = "survival",
            range.v = c(-11, 8),
            adj.ann.subgrp = 4,
            font.size = c(1, 1, .6, .6, .6),
            lab.xy = label.xy)
dev.off()

