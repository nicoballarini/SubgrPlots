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
dat$age1 = factor(dat$age1)
dat %>%
  rename(Stage = stage,
         Performance = pf,
         `Bone\nmetastasis` = bm,
         `History of\ncardiovascular events` = hx) -> dat

###############################################################################-
## 3. Venn Diagram -------------------------------------------------------------
pdf("paper/figures/04-C-venn-diagram-proportional.pdf", width = 5, height = 4,  onefile = FALSE)
plot_venn(dat,
          covari.sel = c(5,7,4),
          cat.sel = c(2,2,2),
          trt.sel = 3,
          resp.sel = c(1,2),
          outcome.type = "survival",
          fill = TRUE,
          range.strip = c(-3, 3),
          n.brk = 31, n.brk.axis = 7,
          font.size = c(1, 1.29, 1.4, 1, 1, 1),
          strip = paste("Treatment effect size (log hazard ratio)"),
          palette = "hcl", prop_area = TRUE)
dev.off()

