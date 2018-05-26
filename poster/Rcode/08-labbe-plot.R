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

covari.sel = c(14,15)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"


covari.sel = c(4,5,6,7)
trt.sel = 3
resp.sel = c(1,2)
outcome.type = "survival"
head(dat)
main.title = "L'Abbe Plot"
title = main.title
lab.xy = list("Control Grp. Estimate x",  "Treatment Grp. Estimate y")
size.shape = 0.2
font.size = c(1, 1, 0.85, 0.85)
adj.ann.subgrp = 1/30
dat %>%
  mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
         hx = factor(ifelse(hx == 0 , "No", "Yes")))-> dat

###############################################################################-
## 8. Labbe Plot -----------------------------------------------------------
pdf("paper/figures/08-labbe-plot-rmst.pdf", width = 6, height = 6)
main.title = "L'Abbe Plot"
main.title = NULL
lab.xy = list("Control Group Estimate",
              "Treatment Group Estimate")
plot_labbe(dat = dat,
           covari.sel = c(4,5,6,7),
           trt.sel = 3,
           resp.sel = c(1,2),
           outcome.type = "survival",
           effect = "RMST",
           title = main.title,
           lab.xy = lab.xy,
           size.shape = 0.2,adj.ann.subgrp = 1/30,
           time=50)
dev.off()

pdf("paper/figures/08-labbe-plot-hr.pdf", width = 6, height = 6)
plot_labbe(dat = dat,
           covari.sel = c(4,5,6,7),
           trt.sel = 3,
           resp.sel = c(1,2),
           outcome.type = "survival",
           effect = "HR",
           title = main.title,
           lab.xy = lab.xy,
           size.shape = 0.2,adj.ann.subgrp = 1/30,
           time=50)
dev.off()


## 8. Labbe Plot -----------------------------------------------------------
pdf("paper/figures/08-labbe-plot-rmst-noci.pdf", width = 6, height = 6)
main.title = "L'Abbe Plot"
main.title = NULL
lab.xy = list("Control Group Estimate",
              "Treatment Group Estimate")
plot_labbe(dat = dat,
           covari.sel = c(4,5,6,7),
           trt.sel = 3,
           resp.sel = c(1,2),
           outcome.type = "survival",
           effect = "RMST",
           title = main.title,
           lab.xy = lab.xy,
           size.shape = 0.2,adj.ann.subgrp = 1/30,
           time=50, show.ci = FALSE)
dev.off()

