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
library(grid)
# library(SubgroUpSetR)
# Load the data to be used
data(prca)
dat <- prca
vars = data.frame(variable = names(dat), index = 1:length(names(dat)))

###############################################################################-
## 12. SubgroUpSet -----------------------------------------------------------
prca.upset = data.frame(trt = factor(ifelse(prca$rx == 1, "Experimental", "Control")),
                        bm = 1*(prca$bm == 1),
                        pf = 1*(prca$pf == 1),
                        hx = 1*(prca$hx == 1),
                        stage = 1*(prca$stage == 4),
                        age = 1*(prca$age > 75),
                        wt = 1*(prca$weight > 100),
                        survtime = prca$survtime,
                        cens = prca$cens==1)
pdf("paper/figures/12-SubgroUpSetR-plot_t.pdf", width = 8, height = 8, onefile=FALSE)
subgroupset(prca.upset,
            order.by = "freq",
            empty.intersections = "on",
            sets = c("bm", 'pf', "hx"),
            text.scale = 1.5,
            mb.ratio = c(0.25, 0.45,0.30),
            treatment.var = "trt",
            outcome.type = "survival",
            effects.summary = c("survtime", "cens"),
            query.legend = "top", icon = "pm", transpose = TRUE)
dev.off()
