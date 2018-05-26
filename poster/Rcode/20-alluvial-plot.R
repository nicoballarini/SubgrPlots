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
dat$trt = dat$rx
# dat
covari.sel = c(14,15)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
dat$trt2 = ifelse(dat$rx == 1, "treatment", "control")

###############################################################################-
## ***Improvement and alternatives*** -----
###############################################################################-



# TEST ---------
# Alluvial diagrams ----
# Put labels to the variables so that they appear in the plot
dat %>%
  dplyr::select(trt, bm, hx, pf) %>%
  dplyr::group_by(trt, bm, hx, pf) %>%
  dplyr::summarise(freq = n()) -> alldat


alldat %>%
  ungroup() %>%
  mutate(trt = ifelse(trt == 0 , "Control", "Treatment"),
         bm = ifelse(bm == 0 , "No", "Yes"),
         hx = ifelse(hx == 0 , "No", "Yes"),
         pf = ifelse(pf == 0 , "No", "Yes")) -> alldat



alluvial::alluvial(alldat[,1:3], freq = alldat$freq,
                   alpha=1, xw=0.2,
                   col=ifelse(alldat$trt == "treatment", "red", "gray"),
                   layer = alldat$trt != "treatment")


pdf("paper/figures/20-alluvial-plot.pdf", width = 7, height = 5)
dat %>%
  dplyr::select(trt, bm, hx, pf) %>%
  dplyr::group_by(trt, bm, hx, pf) %>%
  dplyr::summarise(Freq = n()) -> alldat
alldat %>%
  ungroup() %>%
  mutate(trt = ifelse(trt == 0 , "Control", "Treatment"),
         bm = ifelse(bm == 0 , "No", "Yes"),
         hx = ifelse(hx == 0 , "No", "Yes"),
         pf = ifelse(pf == 0 , "No", "Yes"))-> alldat

alluvial_new(alldat[,c(1,3,2,4)], freq = alldat$Freq,
                   xw=0.2,cw = 0.12,cex = 1,
                   alpha  = 0.8,
                   col=ifelse(alldat$trt == "Treatment","#1f78b4", "#a6cee3"),
                   layer = alldat$trt  == 1, rotate = 90)
dev.off()




dat %>%
  mutate(survival = factor(ifelse(survtime > 24 , "Yes", "No"), levels = c("No", "Yes")))-> dat
dat %>%
  dplyr::select(trt, bm, hx, pf, survival) %>%
  dplyr::group_by(trt, bm, hx, pf, survival) %>%
  dplyr::summarise(Freq = n()) -> alldat
alldat %>%
  ungroup() %>%
  mutate(trt = ifelse(trt == 0 , "Control", "Treatment"),
         bm = ifelse(bm == 0 , "No", "Yes"),
         hx = ifelse(hx == 0 , "No", "Yes"))-> alldat

alluvial_new(alldat[,5:2], freq = alldat$Freq,
             xw=0.2,cw = 0.12,cex = 1,
             alpha  = 0.8,
             col=ifelse(alldat$survival  == "Yes","#1f78b4", "#a6cee3"),
             layer = alldat$trt  == 1, rotate = 90)



pdf("paper/figures/20-alluvial-plot-surv-t.pdf", width = 5, height = 5)
alluvial_transposed(alldat[,c(5,1,3,2,4)], freq = alldat$Freq,
                    xw=0.2,cw = 0.12,cex = 1,
                    alpha  = 0.8,
                    col=ifelse(alldat$survival  == "Yes",
                               ifelse(alldat$trt  == "Treatment","#80b1d3","#d5e2eb"),
                               ifelse(alldat$trt  == "Treatment","#faa8d2","#fbe0ee")),
                    layer = alldat$trt  == 1, rotate = 90, bottom.mar = 5)

dev.off()

