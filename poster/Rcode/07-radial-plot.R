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

# dat
covari.sel = c(4, 5, 6, 7)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
dat %>%
  mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
         hx = factor(ifelse(hx == 0 , "No", "Yes")))-> dat


pdf("paper/figures/07-radial-plot-naive.pdf", width = 5, height = 5)
main.title = title = NULL
lab.xy = label.xy = list(expression(1/hat(sigma)[hat(delta)[i]]),
                         expression((hat(delta)[i]-hat(delta)[F])/hat(sigma)[hat(delta)[i]]))
plot_radial3(dat,
            covari.sel = c(4, 5, 6, 7),
            trt.sel = 3,
            resp.sel = c(1, 2),
            outcome.type = "survival",
            range.v = c(-7, 6),
            adj.ann.subgrp = 4,
            font.size = c(1, 1, 0.75, 0.8, 0.85),
            title = main.title,
            lab.xy = label.xy)
dev.off()
###############################################################################-
## 7. Radial Plot -----------------------------------------------------------
pdf("paper/figures/07-radial-plot-old.pdf", width = 5, height = 5)
label.xy = list("Standardized treatment effect estimate (log-hazard ratio)",
                "1/SE")
plot_radial(dat,
       covari.sel = c(4, 5, 6, 7),
       trt.sel = 3,
       resp.sel = c(1, 2),
       outcome.type = "survival",
       range.v = c(-8, 8),
       adj.ann.subgrp = 4,
       font.size = c(1, 1, .6, .6, .6),
       title = main.title, lab.xy = label.xy)
dev.off()

pdf("paper/figures/07-radial-plot.pdf", width = 5, height = 5)
main.title = NULL
lab.xy = label.xy = list(expression(1/sqrt(hat(Var)(hat(delta)[i]-hat(delta)[F]))),
                         expression((hat(delta)[i]-hat(delta)[F])/sqrt(hat(Var)(hat(delta)[i]-hat(delta)[F]))))
plot_radial2(dat,
            covari.sel = c(4, 5, 6, 7),
            trt.sel = 3,
            resp.sel = c(1, 2),
            outcome.type = "survival",
            range.v = c(-11, 8),
            adj.ann.subgrp = 4,
            font.size = c(1, 1, .6, .6, .6),
            title = main.title, lab.xy = label.xy)
dev.off()

