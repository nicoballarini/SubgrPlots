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
dat %>%
  mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
         hx = factor(ifelse(hx == 0 , "No", "Yes"))) -> dat


###############################################################################-
## 5. Forest Plot -----------------------------------------------------------
pdf("poster/figures/06-forest-plot.pdf", width = 10, height = 6, onefile = F)
main.title = list("", "Forest plot of subgroups",
                  "Kaplan-Meier curves")
label.x = list("", "Log hazard ratio",
               "Time (days)")
plot_forest2(dat,
             covari.sel = c(4,5,6,7),#vars
             trt.sel = 3,
             resp.sel = c(1, 2),
             outcome.type = "survival",
             size.shape = c(0.3, 10/6),
             font.size = c(1, .9, .9, 1),
             title = main.title,
             lab.x = label.x, time = 50, KM = TRUE, pdf = TRUE,
             show.km.axis = 2,
             widths = c(1,0.9,0.7), max.time = 77, n.brk = 12)
dev.off()
