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
levels(dat$age_group) = c("Young", "Middle-aged", "Old")
levels(dat$weight_group)  = c("Low", "Mid", "High")
dat %>%
  mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
         hx = factor(ifelse(hx == 0 , "No", "Yes"))) %>%
  rename(Weight = weight_group,
         Age = age_group) -> dat

###############################################################################-
## 8. Labbe Plot -----------------------------------------------------------
pdf("paper/figures/09-labbe-plot.pdf", width = 5.5/.7, height = 5)
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
           font.size = c(1, 1, 1, 1),
           time=50, show.ci = FALSE,
           legend.position = "outside")
dev.off()

