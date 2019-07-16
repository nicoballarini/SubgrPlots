###############################################################################-
## This program creates the figures for the manuscript using the
## prca data that is included in this package
##
##
## Instead of using rm(list = ls()), make sure you start with a fresh R
## by restarting R -> Control+Shift+F10

## Load needed libraries
## If SubgrPlots package is not installed, then open project and use following
## lines or in the build window, click Install and Restart
# devtools::build()
# devtools::install()
library(SubgrPlots) # Loads this package. Install it first
library(survival)
library(ggplot2)
library(dplyr)

dat <- prca
levels(dat$age_group) = c("Young","Middle-aged","Old")
levels(dat$weight_group)  = c("Low","Mid","High")
comb_levels = c("Young - Low", "Young - Mid", "Young - High",
                "Middle-aged - Low", "Middle-aged - Mid", "Middle-aged - High",
                "Old - Low", "Old - Mid", "Old - High")
dat %>%
  mutate(AgeWeight = factor(sprintf("%s - %s", age_group, weight_group),
                            levels = comb_levels))  %>%
  mutate(survival = factor(ifelse(survtime > 24 , "Yes", "No"),
                           levels = c("No", "Yes"))) %>%
  mutate(rx = factor(rx, labels = c("Control", "Treatment"))) -> dat

pdf("paper/figures/16-nightingale-rose-by-trt.pdf", width = 9, height = 5)
plot_nightingale(dat = dat, trt.sel = 3, covari.sel = 16,
                 resp.sel = 17,
                 seq_by = 25,
                 strip = "2-year survival")
dev.off()
