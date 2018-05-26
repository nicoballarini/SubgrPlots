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
covari.sel =c(4, 5, 7)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
add.aux.line = TRUE
font.size = c(12, 8, 0.35)
main.title = paste("Tree plot for treatment effect difference across subgroups", sep = "");
lab.y.title = paste("Effect size (log hazard ratio)");
title = main.title
lab.y = lab.y.title

###############################################################################-
## 6. Tree Plot -----------------------------------------------------------
pdf("paper/figures/06-tree-plot.pdf", width = 5, height = 5)
main.title = paste("Tree plot for treatment effect difference across subgroups", sep = "");
lab.y.title = paste("Effect size (log hazard ratio)");
main.title = NULL
plot_tree(dat,
        covari.sel = c(4, 5, 7),
        trt.sel = 3,
        resp.sel = c(1, 2),
        outcome.type = "survival",
        add.aux.line = TRUE,
        font.size = c(12, 8, 0.55),
        title = main.title,
        lab.y = lab.y.title,
        keep.y.axis = TRUE)
dev.off()


plot_tree(dat,
          covari.sel = c(4, 5, 7),
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          add.aux.line = TRUE,
          font.size = c(12, 8, 0.55),
          title = main.title,
          lab.y = lab.y.title,
          keep.y.axis = FALSE)

dat %>%
  mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
         hx = factor(ifelse(hx == 0 , "No", "Yes"))) -> alldat


pdf("paper/figures/06-tree-plot.pdf", width = 5, height = 5)
main.title = paste("Tree plot for treatment effect difference across subgroups", sep = "");
lab.y.title = paste("Effect size (log hazard ratio)");
main.title = NULL
plot_tree(alldat,
          covari.sel = c(4, 5, 7),
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          add.aux.line = TRUE,
          font.size = c(12, 8, 0.55),
          title = main.title,
          lab.y = lab.y.title,
          keep.y.axis = TRUE)
dev.off()

pdf("paper/figures/06-tree-plot_freey.pdf", width = 5, height = 5)
main.title = paste("Tree plot for treatment effect difference across subgroups", sep = "");
lab.y.title = paste("Effect size (log hazard ratio)");
main.title = NULL
plot_tree(alldat,
          covari.sel = c(4, 5, 7),
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          add.aux.line = TRUE,
          font.size = c(12, 8, 0.55),
          title = main.title,
          lab.y = lab.y.title,
          keep.y.axis = FALSE)
dev.off()
