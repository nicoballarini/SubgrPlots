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

# # Load the data to be used
data(prca)
dat <- prca
dat %>%
  mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
         hx = factor(ifelse(hx == 0 , "No", "Yes"))) -> dat

pdf("paper/figures/07-B-tree-plot_freey.pdf", width = 5, height = 4.5)
plot_tree(dat,
          covari.sel = c(4, 5, 7),
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          add.aux.line = TRUE,
          font.size = c(12, 12, 0.8),
          title = NULL, text.shift = 0.01,
          lab.y = "Effect size (log hazard ratio)",
          keep.y.axis = FALSE)
dev.off()
