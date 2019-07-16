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
dat$trt = dat$rx
dat %>%
  dplyr::select(trt, bm, hx, pf) %>%
  dplyr::group_by(trt, bm, hx, pf) %>%
  dplyr::summarise(Freq = n()) -> alldat
alldat %>%
  ungroup() %>%
  mutate(trt = ifelse(trt == 0 , "Control", "Treatment"),
         bm = ifelse(bm == 0 , "No", "Yes"),
         hx = ifelse(hx == 0 , "No", "Yes"))-> alldat

pdf("paper/figures/18-alluvial-plot.pdf", width = 5, height = 5)
plot_alluvial(alldat[,c(1,3,2,4)], freq = alldat$Freq,
                   xw=0.2,cw = 0.12,cex = 1,
                   alpha  = 0.8,
                   col=ifelse(alldat$trt == "Treatment","#1f78b4", "#a6cee3"),
                   layer = alldat$trt  == 1, rotate = 90)
dev.off()
