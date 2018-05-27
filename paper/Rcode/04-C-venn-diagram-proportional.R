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

###############################################################################-
## 3. Venn Diagram -------------------------------------------------------------
pdf("paper/figures/04-C-venn-diagram-proportional.pdf", width = 6, height = 5,  onefile = FALSE)
plot_venn_proportional(dat,
          covari.sel = c(5,7,4),
          cat.sel = c(2,2,2),
          trt.sel = 3,
          resp.sel = c(1,2),
          outcome.type = "survival",
          outside.area = FALSE,
          range.strip = c(-3, 3),
          n.brk = 31, n.brk.axis = 7,
          font.size = c(0.5, 0.5, 0.7, 0.5, 0.6, 0.6),
          strip = paste("Treatment effect size (log hazard ratio)"),
          fill = T, fill.background = T, palette = "hcl")
dev.off()
