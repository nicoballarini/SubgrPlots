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
library(ggplot2)
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

pdf("paper/figures/08-galbraith-plot-naive.pdf", width = 5/.70, height = 5)
ggplot_radial(dat,
              covari.sel = c(4, 5, 6, 7),
              trt.sel = 3,
              resp.sel = c(1, 2),
              outcome.type = "survival",
              range.v = c(-8, 6),
              font.size = 4,
              lab.xy = "default",
              ticks.length = 0.05) -> my_plot
my_plot +
  theme(text = element_text(size = 14))
dev.off()


pdf("paper/figures/08-galbraith-plot.pdf", width = 5/.7, height = 5)
ggplot_radial2(dat,
              covari.sel = c(4, 5, 6, 7),
              trt.sel = 3,
              resp.sel = c(1, 2),
              outcome.type = "survival",
              range.v = c(-11, 9),
              font.size = 4,
              lab.xy = "default",
              ticks.length = 0.05) -> my_plot
my_plot +
  theme(text = element_text(size = 14))
dev.off()

