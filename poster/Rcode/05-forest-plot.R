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

dat
covari.sel = c(4,5,6,7)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
main.title = list("", "Forestplot of subgroups", "Forestplot of subgroups (T | C)")
label.x = list("", "Log hazard ratio", "Survival probability at t=50")

size.shape = c(0.25, 0.12)
font.size = c(1, 1, 1, 0.75)
title = main.title
lab.x = label.x
time = 50
new=TRUE

dat %>%
  mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
         hx = factor(ifelse(hx == 0 , "No", "Yes"))) -> dat


###############################################################################-
## 5. Forest Plot -----------------------------------------------------------
pdf("paper/figures/05-forest-plot.pdf", width = 6.5, height = 4)
main.title = list("", "Forestplot of subgroups", "Forestplot of subgroups (T|C)")
label.x = list("", "Log hazard ratio", "Survival probability at t=50")
plot_forest(dat,
          covari.sel = c(4,5,6,7),#vars
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          size.shape = c(0.1, 6.5/4),
          font.size = c(1, 1, 0.7, 0.6),
          title = main.title,
          lab.x = label.x, time = 50)
dev.off()

pdf("paper/figures/05-forest-plot2.pdf", width = 6.5, height = 4, onefile = F)
main.title = list("", "Forest plot of subgroups", "Forestplot of subgroups (T|C)")
label.x = list("", "Log hazard ratio", "Survival probability at t=50")
plot_forest2(dat,
            covari.sel = c(4,5,6,7),#vars
            trt.sel = 3,
            resp.sel = c(1, 2),
            outcome.type = "survival",
            size.shape = c(0.3, 6.5/4),
            font.size = c(0.7, 0.6, 0.4, 0.6),
            title = main.title,
            lab.x = label.x, time = 50, KM = FALSE, pdf = TRUE,
            widths = c(1,1,1))
dev.off()

pdf("paper/figures/05-forest-plot.pdf", width = 5, height = 4, onefile = F)
main.title = list("", "Forest plot of subgroups",
                  "Kaplan-Meier curves\n by treatment group")
label.x = list("", "Log hazard ratio",
               "Time (days)")
plot_forest2(dat,
             covari.sel = c(4,5,6,7),#vars
             trt.sel = 3,
             resp.sel = c(1, 2),
             outcome.type = "survival",
             size.shape = c(0.3, 6.5/4),
             font.size = c(0.6, 0.5, 0.4, 0.6),
             title = main.title,
             lab.x = label.x, time = 50, KM = TRUE, pdf = TRUE,
             show.km.axis = 2,
             widths = c(1,1,0.6))
dev.off()
pdf("paper/figures/05-forest-plot3.pdf", width = 6.5, height = 4, onefile = F)
main.title = list("", "Forest plot of subgroups",
                  "Kaplan-Meier curves by treatment group")
label.x = list("", "Log hazard ratio",
               "Time (days)")
plot_forest2(dat,
             covari.sel = c(4,5,6,7),#vars
             trt.sel = 3,
             resp.sel = c(1, 2),
             outcome.type = "survival",
             size.shape = c(0.3, 6.5/4),
             font.size = c(0.7, 0.6, 0.4, 0.6),
             title = main.title,
             lab.x = label.x, time = 50, KM = TRUE, pdf = TRUE, show.km.axis = 1)
dev.off()
