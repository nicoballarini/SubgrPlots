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
# Load the data to be used
data(prca)
dat <- prca
dat %>%
  rename(Performance = pf,
         `Bone\nmetastasis` = bm,
         `History of\ncardiovascular\nevents` = hx) -> dat
###############################################################################-
## 3. Venn Diagram -----------------------------------------------------------
pdf("paper/figures/04-A-venn-diagram.pdf", width = 5, height = 5)
plot_venn(dat,
          covari.sel = c(5, 7, 4),
          cat.sel  = c(2,2,2),
          trt.sel  = 3,
          resp.sel = c(1,2),
          outcome.type = "survival",
          fill      = FALSE,
          cat.dist  = c(.03,.04,0.08),
          font.size = c(1, 1.29, 1.4, 1, 1, 1))
dev.off()
