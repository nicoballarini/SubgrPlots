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

## Load the data to be used
data(prca)
dat <- prca
dat$age1 = factor(dat$age1)
dat %>%
  rename(Performance = pf,
         `Bone\nMetastasis` = bm,
         `History of\ncardiovascular\nevents` = hx) -> dat

pdf("paper/figures/04-B-venn-diagram-filled.pdf", width = 6, height = 5,  onefile = FALSE)
plot_venn_fill(dat,
               covari.sel = c(4,6,7,5),#vars,
               cat.sel = c(2,2,2,2),
               trt.sel = 3,
               resp.sel = c(1,2),
               outcome.type = "survival",
               outside.area = FALSE,
               range.strip = c(-24, 24),
               n.brk = 25,
               n.brk.axis = 13,
               font.size = c(0.5, 0.5, 0.7, 0.5, 0.6, 0.6),
               strip = paste("Treatment effect size (log hazard ratio)"), palette = "hcl")
dev.off()
